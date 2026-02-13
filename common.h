#ifndef quadc_common_h
#define quadc_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#define DEBUG_PRINT_CODE

/*
    When this flag is defined, the VM disassembles and prints each instruction right before executing it. Where our previous disassembler walked an entire chunk
    once, statically, this disassembles instructions dynamically, on the fly
*/
#define DEBUG_TRACE_EXECUTION

#define DEBUG_STRESS_GC

#define DEBUG_LOG_GC

#define UINT8_COUNT (UINT8_MAX + 1)

#endif