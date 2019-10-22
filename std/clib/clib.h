#ifndef __CLIB_H
#define __CLIB_H

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define i64 int64_t
#define i8 int8_t
#define i1 bool

struct array {
  i64 length;
  i8* element_size;
  i8* pointer;
};

struct string {
  i64 length;
  i8* pointer;
};

inline void _ensure_bounds_or_panic(const char* type, i64 size, i64 idx) {
  if (idx < 0 || idx >= size) {
    fprintf(stderr, "PANIC: Tried to deref %s (size=%ld) at index %ld\n",
            type, size, idx);
    exit(1);
  }
}

#endif // __CLIB_H