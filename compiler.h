#ifndef quadc_compiler_h
#define quadc_compiler_h

#include "vm.h"

#include "object.h"

ObjFunction* compile(const char* source);

void markCompilerRoots();

#endif 