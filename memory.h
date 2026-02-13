#ifndef quadc_memory_h
#define quadc_memory_h

#include "common.h"
#include "object.h"

#define ALLOCATE(type, count) \
    (type*) reallocate(NULL, 0, sizeof(type) * (count))

/*
    Using reallocate() to free memory might seem pointless. Why not just call free()? Later, this will help the VM track how much memory is still being used. If all allocation and freeing goees through 
    reallocate(), it's easy to keep a running count of the number of bytes of allocated memory.
*/
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)
    
#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

void* reallocate(void* pointer, size_t oldSize, size_t newSize);

void markObject(Obj* object);

void markValue(Value value);

void collectGarbage();

void freeObjects();

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0);

#endif