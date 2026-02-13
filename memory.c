#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    vm.bytesAllocated += newSize - oldSize;

    if (newSize > oldSize) {
        #ifdef DEBUG_STRESS_GC
            collectGarbage();
        #endif
        if (vm.bytesAllocated > vm.nextGC) {
            collectGarbage();
        }
    }
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);

    //not enough memory
    if(result == NULL) exit(1);
    return result;
}

void markObject(Obj* object) {
    
    if (object == NULL) return;
    // Check for cycles, if an object has already been marked then we don't want to get caught in a loop.
    if(object->isMarked) return;
    #ifdef DEBUG_LOG_GC
        printf("%p mark ", (void*) object);
        printValue(OBJ_VAL(object));
        printf("\n");
    #endif
    object->isMarked = true;

    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity); 
        
        if(vm.grayStack == NULL) exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
}


void markValue(Value value) {
    if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
    for (int i = 0; i < array-> count; i++) {
        markValue(array->values[i]);
    }
}

static void blackenObject(Obj* object) {
    
    switch (object->type) {
        printf("%p blacken ", (void*) object);
        printValue(OBJ_VAL(object));
        printf("\n");
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = (ObjBoundMethod*) object;
            markValue(bound->receiver);
            markObject((Obj*)bound->method);
            break;
        }
        /*
            If the instance is alive, we need to keep its class around. Also, we need to keep every object referenced by the instance's fields. Most live objects that are not roots are reachable because some instance
            refers to the object in a field. 
        */
        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            markObject((Obj*)instance->klass);
            markTable(&instance->fields);
            break;
        }
        /*
            When the GC reaches a class object, it marks the class's name to keep that string alive too.
        */
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*) object;
            markObject((Obj*)klass->name);
            markTable(&klass->methods);
            break;
        }
        /*
            Each closure has a reference to the bare function it wraps, as well as an array of pointers to the upvalues it captures. We trace all of those
        */
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*) object;
            markObject((Obj*)closure->function);
            for (int i = 0; i < closure -> upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        /*
            Each function has a reference to an ObjString containing the function's name. More importantly, the function has a constant table packed full of references to other objects. We trace all of those with markArray().
        */
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*) object;
            markObject((Obj*) function -> name);
            markArray(&function->chunk.constants);
            break;
        }
        /*
            when an upvalue is closed, it contains a reference to the closed-over value. Since the value is no longer on the stack, we need to make sure we trace the reference to it from the upvalue.
        */
        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object) -> closed);
            break;
        /*
            the easy ones - strings and native function objects contain no outgoing references so there is nothing to travers.
        */
        case OBJ_NATIVE:
        case OBJ_STRING:
            break;
    }
}

static void freeObject(Obj* object) {
    #ifdef DEBUG_LOG_GC
        printf("%p free type %d\n", (void*) object, object->type);
    #endif
    switch(object->type) {
        case OBJ_BOUND_METHOD: 
            FREE(ObjBoundMethod, object);
            break;
        /*
            The instance owns its field table so when freeing the instance, we also free the table. We don't explicityly free the entries in the table, because there
            may be other references to those objects. The garbage collector will take care of those for us. Here we free only the entry array of the table itself.
        */
        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*) object;
            freeTable(&instance->fields);
            FREE(ObjInstance, object);
            break;
        }
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*) object;
            freeTable(&klass->methods);
            FREE(ObjClass, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*) object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_NATIVE: 
            FREE(ObjNative, object);
            break;
        case OBJ_UPVALUE: 
            FREE(ObjUpvalue, object);
            break;
        case OBJ_STRING: {
            ObjString* string = (ObjString*) object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
    }
}

static void sweep() {
    Obj* previous = NULL;
    Obj* object = vm.objects;
   
    //walk the list of objects and check their marked bits
    while (object != NULL) {
        if (object-> isMarked) {
            //reset for next garbage collection
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object -> next;
            if (previous != NULL) {
                previous -> next = object;
            } else {
                vm.objects = object;
            }
            freeObject(unreached);
        }
    }
}



/*
    Most roots are local variables or temporaries sitting right in the VM's stack, so we start by walking that
*/
static void markRoots() {
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }
    /*
        Most function call state lives in the value stack, but the VM maintains a separate stack of CallFrames. Each CallFrame contains a pointer to the closure
        being called. The VM uses those pointers to access contants and upvalues, so those closures need to be kept around too
    */
    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj*) vm.frames[i].closure);
    }
    // The open upvalue list is another set of values that the VM can directly reach
    for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }
    //marking the stack takes care of local variables and temporaries. The other main source of roots are the global variables
    markTable(&vm.globals);
    markCompilerRoots();
    markObject((Obj*)vm.initString);
}

static void traceReferences() {
    /*
        Until the stack empties, we keep pulling out gray objects,, traversing their references, and then marking them black. Traversing an object's references may turn up
        new white objects that get marked gray and added to the stack. So this function swings back and forth between turning white objects gray and gray objects black
    */
    while(vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack); 
}

//Commented out garbage collector debug notes cause it was kind of annoying feel free to uncomment.
void collectGarbage() {
    // #ifdef DEBUG_LOG_GC
    //     printf("-- gc begin\n");
    //     size_t before = vm.bytesAllocated;
    // #endif
    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings);
    sweep();
    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;
    // #ifdef DEBUG_LOG_GC
    //     //printf("-- gc end\n");
    //     printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
    //      before - vm.bytesAllocated, before, vm.bytesAllocated,
    //      vm.nextGC);
    // #endif
}