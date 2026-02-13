#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "debug.h"
#include "memory.h"

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

//lowest to highest precedence
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, //=
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISION, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * / 
    PREC_UNARY, // ! -
    PREC_CALL, // . () 
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;


typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

/*
    The index field stores which local slot the upvalue is capturing. 
*/
typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
    TYPE_METHOD,
    TYPE_INITIALIZER
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    //Use this to get chunk
    ObjFunction* function;
    FunctionType type;
    
    Local locals[UINT8_COUNT];
    
    // how many locals are in scope
    int localCount;

    // zero is global, one is top-level, two is inside that, etc.
    int scopeDepth;

    Upvalue upvalues[UINT8_COUNT];
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;
ClassCompiler* currentClass = NULL;

static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);
    
    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end") ;
    } else if(token->type == TOKEN_ERROR) {
        // Nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for(;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    //simple validation
    errorAtCurrent(message);
}


static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() {
    if (current -> type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }
    emitByte(OP_RETURN);
}

static ObjFunction* endCompiler() {
    ObjFunction* function = current->function;
    emitReturn();
    #ifdef DEBUG_PRINT_CODE
        if (!parser.hadError) {
            disassembleChunk(currentChunk(), function->name != NULL ? function -> name -> chars: "<script>");
        }
    #endif
    current = current -> enclosing;
    return function;
}

//to create a scope all we do is increment the current depth
static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}


static void markInitialized() {
    if (current->scopeDepth == 0) return;
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);


static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->function = NULL;
    compiler->type = type;
    compiler->enclosing = current;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
         local->name.start = "";
         local->name.length = 0;
    }
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if( constant > UINT8_MAX){
        error("Too many constants in one chunk");
        return 0;
    }

    return (uint8_t) constant;
}

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}



static int resolveLocal(Compiler* compiler, Token* name) {
   
    for (int i = compiler->localCount -1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if(local->depth == -1) {
                error("Can't read local variable in its own initializer");
            }
            return i;
        }
    }

    return -1;
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler -> function -> upvalueCount;
    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue -> index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if(upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function");
        return 0;
    }
    compiler -> upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler -> enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t) local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t) upvalue, false);
    }


    return -1;
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }
    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
    local->depth = current->scopeDepth;
}

static void declareVariable() {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount -1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current ->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope");
        }
    }
    addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);
    declareVariable();
    // exit function if we're in a local scope
    if (current->scopeDepth > 0) return 0;
    return identifierConstant(&parser.previous);
}

static void synchronize() {
    parser.panicMode = false;
    while (parser.current.type != TOKEN_EOF){
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch(parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default: //do nothing
        }
        
        advance();
    }
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while(match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0) ;
        emitByte(compiler.upvalues[i].index);
    }
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name");
    uint8_t constant = identifierConstant(&parser.previous);
    
    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    function(type);
    emitBytes(OP_METHOD, constant);
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp,(uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void patchJump(int offset) {
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset+1] = jump & 0xff;
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count -2;
}


static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}


/*
    Similar to emitJump() and patchJump() combined. It emits a new loop instruction, which unconditionally jumps backwards by a given offset. Like 
    the jump instructions, after that we have a 16-bit operand. WE calculate the offset from the instruction we're currently at to the loopStart point 
    that we want to jump back to. The +2 is to take into account the size of the OP_LOOP instruction's own operands which we also need to jump over.
    From the VM's perspective, there really is no semantic difference between OP_LOOP and OP_JUMP. Both just add an offset to the ip. We could use a 
    single instruction for both and given it a signed offset operand (but there's some annoying stuff for that to work.)
*/
static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk() -> count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop Body too Large");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

/*
    Functions are first-class values, and a function declaration simply creates and stores one in a newly declared variable. So we parse the name like any other variable declaration. A function declaration at the top level
    will bind the function to a global variable. Inside a block or other function, a function declaration creates a local variable.
    Functions don't suffer the same problem as variables -> we don't need to worry about them refrencing themselves in their own initializer. It's safe for a function to refer to its own name inside its body. You can't call
    the function and execute the body until after it's fully defined. To make that work, we mark the function declaration's variable "initialized" as soon as we compile the name, before we compile the body. 
    That way the name can be referenced inside the body without generating an error
*/
static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    /*
        After we inherit the class name, if the next token is a <, then we found a superclass clause. We consume the superclass's identifier token, then call variable(). That function takes the previously consumed token, treats it as a 
        variable reference, and emits code to load the variable's value. In other words, it looks up the superclass by name and pushes it onto the stack. After that, we call namedVariable() to load the subclass doing the inheriting onto the stack, 
        followed by an OP_INHERIT instruction. That instruction wires up the superclass to the new subclass. The OP_INHERIT instruction takes an existing class and applies the effect of inheritance to it.
    */
    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name");
        variable(false);
        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself.");
        }
        namedVariable(className, false);
        emitByte(OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    /*
        Creating a new lexical scope ensures that if we declare two classes in the same scope, each has a different local slot to store its superclass. Since we always name this variable "super", if we didn't make a scope for each subclass, the variables
        would collide. We name the variable "super" for the same reason we use 'this' as the name of the hidden local variable that this expressions resolve to: 'super' is a reserved word, which guarentees the compilers hidden variable won't collide with a 
        user-defined one. The difference is that when compiling this expressions we conveniently have a token sitting around whose lexeme is 'this'. We aren't so lucky here. Instead, we add a little helper function to create a synthetic tokenf or the given 
        constant string (syntheticToken())
    */
    beginScope();
    addLocal(syntheticToken("super"));
    defineVariable(0);

    // Load the class back on top of the stack. namedVariable() generates code to load a variable with the given name onto the stack.
    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body");

    while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body");
    // Once we've reached the end of the methods, we no longer need the class and tell the VM to pop it off the stack.
    emitByte(OP_POP);

    // Since we opened a local scope for the superclass variable, we need to close it. We pop the scope and discard the 'super' variable after compiling the class body and its methods. That way, the variable is accessible in all of the methods of the subclass
    if(classCompiler.hasSuperclass) {
        endScope();
    }

    /*
        The memory for the ClassCompiler struct lives right on the C stack, At the end of the class body, we pop that compiler off the stack and restore the enclosing one. When an outermost class body ends, enclosing will be NULL, so this resets currentClass 
        to NULL. Thus, to see if we are inside a class - and therefore inside a method - we check the module variable.
    */
    currentClass = currentClass -> enclosing;
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }
    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer");
        }
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value");
        emitByte(OP_RETURN);
    }
}

static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
    defineVariable(global);
}


/*
    An "expression statement" is simply an expression followed by a semicolon. They're how you write an expression in a context where a statement is expected. Usually, it's so that you can call a function
    or evaluate an assignment for its side effect like:
        brunch = "quich";
        eat(brunch);
    Semantically, an expression statement ecvaluates the expression and discards the result. The compiler directly encodes that behavior. It compiles the xpression, and then exmit an OP_POP instruction
*/
static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression");
    emitByte(OP_POP);
}

static void forStatement() {
    // if a for statement declares a variable, that variable should be scoped to the loop body
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    //declaration or expression
    if (match(TOKEN_SEMICOLON)) {
        // No initializer
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;
    /*
        Since the clause if optional, we need to see if it's actually present. If the clause is omitted, the next token must be a semicolon, 
        so we look for that to tell. If there isn't a semicolon, there must be a condition expression. In that case, we compile it. Then, just like 
        with while, we emit a conditional jump that exits the loop if the condition is falsey. Since the jump leaves the value on the stack, we pop it before
        executing the body. That ensures we discard the value when the condition is true
    */
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition");

        // Jump out of the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }
    /*
        Again, it's optional. Since this is the last clause, when omitted, the next token will be the closing parenthesis. When an increment is present, we need to
        compile it now, but it shouldn't execute yet. So, first, we emit an unconditional jump that hops over the increment clause's code to the body of the loop. Next
        we compile the increment expression itself. This is ususally an assignment. Whatever it is, we only execute it for its side effect, so we also emit a pop to discard the value.
        The last part is trick. First, we emit a loop instruction. This is the main loop that takes us back to the top of the for loop - right before the conditional expression
        if there is one. That loop happens right after the increment, since the increment executes at the end of each loop interation. Then we change loopStart to point to the offset
        where the increment expression begins. Later, when we emit the loop instruction after the body statement, this will cause it to jump up to the increment expression instead of the top of
        hte loop like it does when there is no increment. 
    */
    if (!match(TOKEN_RIGHT_PAREN)){
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk() -> count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }
    statement();
    emitLoop(loopStart);
    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);
    }
    endScope();
}

/*
    First we compile the condition expression, bracketed by parentheses. At runtime, that will leave the condiition value on top of the stack. We'll use that to determine whether to execute the then branch 
    or skip it. Then we emit a new OP_JUMP_IF_FALSE instruction. It has an operand for how much to offset the ip - how many bytes of code to skip. If the condition is falsey, it adjusts the ip by that amount. 
    We get that amount through backpatching. We emit the jump instruction first with a placeholer offset operand. We keep track of where the half-finished instruction is. Next, we compile the then body. Once
    that's done, we know how far to jump. So we go back and replace that placeholder offset with the real one now that we can calculatte it.
*/
static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Exprect, ')' after condition");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    emitByte(OP_POP);

    /*
        When the condition is falsey, we'll jump over the then branch. If there's an else branch, the ip will land right at the beginning of its code. Buts thats not enough we also have to account for when its truthy
        and the then branch falls through. To implement this we need another jump from the end of the then branch
    */
    if (match(TOKEN_ELSE)) statement();
    /*
        Unline the other jump, this jump is unconditional. We always take it, so we need another expression that expresses that.
    */
    patchJump(elseJump);
}

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)){
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

/*
    We treat this as a lexically scoped local variable whose value get magically initialized. Compiling it like a local variable means we get a lot of behavior for free. In particular, closures inside amethod that 
    reference this will do the right thing and capture the receiver in an upvalue. When the parser function is called, the this token has just been consumed and is stored as the previous token. We call our existing 
    variable() function which compiles identifier expression as variable accesses. It takes a single boolean parameter for whether the compiler should look for a following '=' opoerator and parse a setter. You can't 
    reassign to this, so we pass false to disallow that.
*/
static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class");
        return;
    }
    variable(false); 
}

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence) (rule->precedence + 1));

    switch(operatorType) {
        case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
        case TOKEN_GREATER: emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS: emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL: emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS: emitByte(OP_ADD); break;
        case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
        default: return; //Unreachable
    }
}

// leading '-' has already been consumed
static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    //compile the operand
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction
    switch(operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return; //Unreachable
    }

}

/*
    At the point this is called, the left-hand side expression has already been compiled. That means at runtime, its value will be on top of the stack. If that value is falsey, then we know the entire 
    and must be false, so we skip the right operand and leave the left-hand side value as the result of the entire expression. Otherwise, we discard the left-hand value and evaluate the right operand which 
    becomes the result of the whole and expression 
*/
static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    parsePrecedence(PREC_AND);
    patchJump(endJump);
}


/*
    This takes the string's characters directly from the lexeme. The '+1' and '-2' parts trim the leading and trailing quotations marks. It then creates a string object, wraps it in a 
    Value, and stuffs it into the constant table
*/
static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, 
        parser.previous.length-2)));
}


/*
    We compile the condition expression, surrounded by mandatory parenthesis. That's followed by a jump instruction that skips over the subsequent body statement if
    the condition is falsey. We patch the jump after compiling the body and take care to pop the condition value from the stack on either path. After executing the body of a 
    while loop, we jump all the way back to before the condition. That way, we re-evaluate the condition expression on each iteration. We store the chunk's current instruction count
    in loopStart to record the offset in the bytecode right beforee the condition expression we're about to compile. Then we pass that into this helper function
*/
static void whileStatement() {
    int loopStart = currentChunk() -> count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition");
    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    /*
        After the body, we call this function to emit a "loop" instruction. That instrution needs to know how far back to jump. When jumping forward, we had to emit the instruction 
        in two stages since we didn't know how far we were going to jump unitl after we emitted the jump instruction. We don't have that problem now. We've already compiled the point in code
        that we want to jump back to - it's right before the condition expression 
    */
    emitLoop(loopStart);
    patchJump(exitJump);
    emitByte(OP_POP);
}

static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }
    if(parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if(match(TOKEN_FOR)) {
        forStatement();
    } else if(match(TOKEN_IF)) {
        ifStatement();
    } else if(match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if(match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();

    /*
        An "expression statement" is simply an expression followed by a semicolon. They're how you write an expression in a context where a statement is expected. Usually, it's
        so that you can call a function or evaluate an assignment for its side effect. Semantically, an expression statement evaluates the expression and discards the result.
    */
    } else {
        expressionStatement();
    }
}

/*
    The parsing function for an expression type can consume any additional tokens that it wants to, just like in a regular recursive descent parser. 
    We assume the initial '(' has already been consumed. we recursively call back into expression() to compile the expression between the parenthesis, then
    parse the closing ')' at the end. As far as the back end is concerned, there's literally nothing to a grouping expression. Its sole function is syntactic - it
    lets you insrt a lower-precedence expression where a higher precedence is expected. Thus, it has no runtime semantics on its own and therefore doesn't emit any 
    bytecode. The inner call to expression() takes care of generating bytecode for the expression inside the parenthesis
*/
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression");
}

/*
    We assume the token for the number literal has already been consumed and is stored in previous. We take that lexeme and use the C standard library to convert it to
    a double value. Then we generate the code to load that value using th emitConstant() function
*/
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

/*
    Unline 'this' expressions, a super token is not a standalone expression. Instead, the dot and method name following it are inserparable parts of the syntax. However, the parenthesized argument list is separate. As 
    with normal memthod access, QuadC supports getting a reference to a superclass method as a closure without invoking it.
    When a compiler hits a super token, we consume the subsequent '.' token and then look for a method name. Methods are looked up dynamically, so we use identifierConstant() to take the lexeme of the method namee toekn and 
    sotre it in the constant table just like we do for property access expressions.
*/
static void super_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'super' outside of a class.");
    } else if(!currentClass->hasSuperclass) {
        error("Can't use 'super' in a class with no superclass");
    }
    consume(TOKEN_DOT, "Expect '.' after 'super'");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint8_t name = identifierConstant(&parser.previous);

    /*
        In order to access a superclass method on the current instance, the runtime needs both the receiver and the superclass of the surrounding method's class. The first namedVariable() call generates code to look up the current receiver
        stored in the hidden variable 'this' and push it onto the stack. The second namedVariable() call emits code to look up the superclass fro its 'super' variable and push that on top. Finally, we emit a new OP_GET_SUPER instruction with an 
        operand for the constant table index of the method name. 
    */
    namedVariable(syntheticToken("this"), false);
    /*
        Look for a parenthesized argument list. If we find one, we compile that. Then we load the superclass. After that, we emit a new OP_SUPER_INVOKE instruction. this superinstruction combines the behavior of OP_GET_SUPER and OP_CALL so it takes two 
        operands: the constant table index of the method name to look up and the number of arguments to pass to it. Otherwise, if we don't find a '(', we continue to compile the expression as a super access like we did before and emit an OP_GET_SUPER
    */
    if (match(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(argCount);
    } else {
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, name);
    }
}


static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
    }
}

// Already consumed the '(' token
static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

/*
    The parser expects to find a property name immediately after the dot. We load that token's lexeme into the constant table as a string so that the name is available at runtime
    If we see an equals sign after the field name, it must be a set expression that is assigning to a field
*/
static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)){
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if(match(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,   PREC_CALL},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_LESS]          = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_IDENTIFIER]    = {variable,     NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {super_,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {this_,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type) -> prefix;
    if (prefixRule == NULL) {
        error("Expect expression");
        return;
    }
    /*
        variable() should look for and consume the '=' only if it's in the context of a low-precedence expression. Since assignment is the lowest-precedence expression, the only time we allow an assignment is when parsing an assignment
        expression or top-level expression like in an expression statement. 
    */
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type) -> infix;
        /*
            prefixRule and infixRule are stored in a table of function pointers, so all of the parse functions need to have the same type. Even though most parse functions don't support being used as an assignment target - setters are the 
            only other one - our C compiler requires all them all to accept the parameter canAssign
        */
        infixRule(canAssign);

    }
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }

}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

ObjFunction*  compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);
    parser.hadError = false;
    parser.panicMode = false;
    advance();
    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

/*
    The compiler itself periodically grabs memory form the heap for literals and the constant table. If the GC runs while we're in the middle of compiler,
    then any values the compiler directly accesses need to be treated as roots too. Fortunately, the compiler doesn't have too many values that it hangs on to. The
    only object it uses is the ObjFunction is it compiling into. Since function declarations can nest, the compiler has a linked list of those and we walk the while list
*/
void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler -> function);
        compiler = compiler->enclosing;
    }
}

