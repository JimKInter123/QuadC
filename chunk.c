#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "value.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
    
}

/*
    The first thing we need to do is see if the current array already has capacity for the new byte. If it doesn't, then we first need to grow the array to make room
    
*/
void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk-> capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count]=byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

/*
    We deallocate all of the memory and then call initChunk() to zero out the fields leaving the chunk in a well-defined empty state.
*/
void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

int addConstant(Chunk* chunk, Value value) {
    /*
        The new object being added to the constant table is passed to addConstant(). At that moment, the object can be found only in the parameter
        to that function on the C stack. That function appends the object to the constant table. If the table doesn't have enough capacity and needs 
        to grow, it calls reallocate(). That in turn triggers a GC which fails to mark the new constant object and thus sweeps it right before we have a 
        chance to add it to the table -> Crash. The fix is to push the constant onto the stack temporarily then once the constant table contains the object, 
        we pop it off the stack.
    */
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count-1;
}