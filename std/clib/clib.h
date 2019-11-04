#ifndef __CLIB_H
#define __CLIB_H

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define i64 uint64_t
#define i32 uint32_t
#define i16 uint16_t
#define i8 uint8_t
#define i1 bool

#define DEBUG_PRINTF(...) if (true) { printf(__VA_ARGS__); }
#define NOINLINE __attribute__((noinline))

struct array {
  i64 length;
  i64 element_size;
  i8 payload[];
};

struct string {
  i64 length;
  i8 payload[];
};

struct callback {};

inline void _ensure_bounds_or_panic(const char* type, i64 size, i64 idx) {
  if (idx < 0 || idx >= size) {
    fprintf(stderr, "PANIC: Tried to deref %s (size=%"PRId64") at index %"PRId64"\n",
            type, size, idx);
    exit(1);
  }
}

void* gc_alloc_block(i64 size, i16 type, i8* cheshire_stack_root);
void gc(i8* cheshire_stack_root);

#endif // __CLIB_H