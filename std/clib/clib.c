#include "clib.h"

// Stuff having to do with garbage collection
#include "gc.c"

// Functions explicitly used by the tr codegen
#include "tr.c"

// Functions that are exported in std/internal/**.ch
// These are, by virtue of their being exported, also decorated.
#include "internal.c"

// Used in LLVM GC strategy
#include "statepoint.c"