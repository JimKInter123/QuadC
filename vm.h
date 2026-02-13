#ifndef quadc_vm_h
#define quadc_vm_h

#include "chunk.h"
#include "value.h"
#include "table.h"
#include "object.h"

#define FRAMES_MAX 64
# define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

/*
    A CallFrame represents a single ongoing function call. The slots field points into the Vm's value stack at the first slot that this 
    function can use. 
    Instead of storing the return address in the callee's frame, the caller stores its own ip. When we return from a function, the VM will jumnp 
    to the ip of the caller's CallFrame and resume from there.
    Also has a pointer to the function (changed to be closure) being called. We'll use that to look up constants and for a few other things
    Each time a function is called, we create on of these structs. We could dynamically allocate them on the heap, but that's slow. Function calls are a core
    operation, so they need to be as fast as possible
*/
typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    // stores the current height of the CallFrame stack - the number of ongoing function calls
    int frameCount;
    Value stack[STACK_MAX];
    Value* stackTop;
    Table strings;
    ObjString* initString;
    ObjUpvalue* openUpvalues;
    // total of the number of bytes of managed memory the VM has allocated.
    size_t bytesAllocated;
    /*
        The threshold that triggers the next collection. As the amount of live memory increases, e collect less frequently in order to avoid sacrificing throughput 
        by re-traversing the groing pile of live objects. As the amount of live memory goes down, we collect more frequently so that we don't lose too much latency by waiting 
        too long.
    */
    size_t nextGC;
    Obj* objects;
    Table globals;
    int grayCount;
    int grayCapacity;
    Obj** grayStack;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

/*
    The "object" module is directly using the global vm variable from the "vm" module, so we need to expose that externally.
*/
extern VM vm;

void initVM();
void freeVM();

//source -> string of source code
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif

