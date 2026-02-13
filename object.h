#ifndef quadc_object_h
#define quadc_object_h

#include "common.h"
#include "value.h"
#include "chunk.h"
#include "table.h"

/*
    Get object type tag from a given value
*/
#define OBJ_TYPE(value) (AS_OBJ(value)->type)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure*) AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*) AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance*) AS_OBJ(value))
#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))
/*
    Ensure that the Obj* pointer you have does point to the obj field of an actual ObjString
*/
#define IS_STRING(value) isObjType(value, OBJ_STRING)
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString*) AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_CLOSURE,
    OBJ_UPVALUE,
    OBJ_STRING,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_BOUND_METHOD
} ObjType;

/*
    We'll create a linked list that stores every Obj. The VM can traverse that list to find every single object that has been allocated on the heap, whether 
    or not the user's program or the VM's stack still has reference to it(it has been popped off the stack). We could define a separate linked list node struct
    but then we'd have to allocate those too. Instead, we'll use an intrusive list - the Objj struct itself will be the linked lsit node. Each Objj gets a pointer
    to the next Obj in the chain. The VM stores a pointer to the head of the list.
*/
struct Obj {
    ObjType type;
    struct Obj* next;
    bool isMarked;
};

/*
    Functions are first class in QuadC, so they need to be actual QuadC objects. Thus ObjFunction has the same Obj header that all object types share. The arity field
    stores the number of parameters the function expects. Then, in addition to the chunk, we store the functions's name (handy for errors). 
*/
typedef struct {
    Obj obj;
    int arity;
    int upvalueCount;
    Chunk chunk;
    ObjString* name;
} ObjFunction;

typedef Value (*NativeFn) (int argCount, Value* args);

typedef struct {
    Obj obj;
    NativeFn function;
} ObjNative;

/*
    A string object contains an array of characters. Those are stored in a separate, heap-allocated array so that we set aside 
    only as much room as needed for each string. We also store the number of bytes in the array. This isn't strictly necessary but lets us
    tell how much memory is allocated for the string without walking the character array to find the null terminator. Because ObjString is an Obj, it
    also needs the state all Objs share. It accomplishes that by having its first field be an Obj. C specifies that struct fields are arranged in memory
    in the order that they are declared. Also, when you nest structs, the inner sturct's fields are expanded right in place. So, the memory for Obj and 
    for ObjString overrlap. This means you can take a pointer to a struct and safely convert it to a pointer to its first field and back. Given an ObjString* 
    you can safely cast it to Obj* and then access they type field from it. Every ObjString "is" an Obj in the OOP sense of "is"
*/
struct ObjString {
    Obj obj;
    int length;
    char* chars;
    /*
        Cache the hash so we don't have to calculate it every time. Every ObjString stores the hash code for its string. Since strings are immutable in QuadC, 
        we can calculate the hash code once up front and be certain that it will never get invalidated. Caching it eagerly makes a kind of sense: allocate the 
        string and copying its characters over is already an O(n) operation, so it's a good time to also do the O(n) calculation of the strings hash 
    */
    uint32_t hash;
};

typedef struct ObjUpvalue {
    Obj obj;
    Value* location;
    struct ObjUpvalue* next;
    Value closed;
} ObjUpvalue;

typedef struct {
    Obj obj;
    ObjFunction* function;
    /*
        Different closures may have different numbers of upvalues, so we need a dynamic array. The upvalues themselves are dynamically allocated too, so we end up with a double pointer
        A pointer to a dynamically allocated array of pointers to upvalues. We also store the number of elements in the array (ObjFunction also keeps count so its redundant but it helps with
        the garbage collector)
    */
    ObjUpvalue** upvalues;
    int upvalueCount;
} ObjClosure;

/*
    In a class-based object-oriented language, everything begins with classes. They define what sorts of objects exist in the program and are the factories used to produce new instanced. Going 
    bottom-up, we'll start with their runtime representation and then hook that into the language.
*/
typedef struct {
    Obj obj;
    ObjString* name;
    Table methods;
} ObjClass;

/*
    Instances know their class - each instance has a pointer to the class that it is an instace of. More important is how instances store their state. QuadC lets users freely add fields to an instance at 
    runtime. This means we need a storage mechanism that can grow. We could use a dynamic array, but we also want to look up fields by name as quickly as possible -> Hash Table
*/
typedef struct {
    Obj obj;
    ObjClass* klass;
    Table fields;
} ObjInstance;

/*
    Wraps the receiver and the method closure together. The receiver's type is Value even though methods can be called only on ObjInstances. Since the VM doesn't care what kind of receiver it has anyway, using Value 
    means we don't have to keep converting the pointer back to a value when it gets passed to more general functions.
*/
typedef struct {
    Obj obj;
    Value receiver;
    ObjClosure* method;
} ObjBoundMethod;

ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* klass);
ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method);
ObjNative* newNative(NativeFn function);
ObjString* copyString(const char* chars, int length);
// Takes the address of the slot where the closed-over variable lives
ObjUpvalue* newUpvalue(Value* slot);
ObjString* takeString(char* chars, int length);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif