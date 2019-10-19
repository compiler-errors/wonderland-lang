#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "statepoint.h"

#define i64 int64_t
#define i8 int8_t
#define i1 bool

struct array {
  i64 size;
  i8* ptr;
};

struct string {
  i64 size;
  i8* ptr;
};

void ensure_bounds_or_panic(const char* type, i64 size, i64 idx) {
  if (idx < 0 || idx >= size) {
    fprintf(stderr, "PANIC: Tried to deref %s (size=%ld) at index %ld\n",
            type, size, idx);
    exit(1);
  }
}

i1 fpP8internalP9operators6eq_int(i64 a, i64 b) {
  return a == b;
}

i1 fpP8internalP9operators6gt_int(i64 a, i64 b) {
  return a > b;
}

i64 fpP8internalP9operators7add_int(i64 a, i64 b) {
  return a + b;
}

i64 fpP8internalP9operators7neg_int(i64 a) {
  return -a;
}

i64 fpP8internalP9operators7mul_int(i64 a, i64 b) {
  return a * b;
}

i64 fpP8internalP9operators7div_int(i64 a, i64 b) {
  return a / b;
}

i64 fpP8internalP9operators7mod_int(i64 a, i64 b) {
  return a % b;
}

struct string* alloc_string(i8* string, i64 size) {
  i8* string_copy = calloc(size + 1, sizeof(i8));
  memcpy(string_copy, string, size * sizeof(i8));

  struct string* string_ptr = malloc(sizeof(struct string));
  string_ptr->ptr = string_copy;
  string_ptr->size = size;

  return string_ptr;
}

struct string* fpP8internalP16transmute_string15int_into_string(i64 i) {
  i8 buf[21] = {0};
  i64 size = sprintf((char*) buf, "%ld", i);

  return alloc_string(buf, size);
}

struct string* fpP8internalP16transmute_string16char_into_string(i8 c) {
  return alloc_string(&c, 1);
}

struct string* fpP8internalP9operators10add_string(struct string* a,
                                                   struct string* b) {
  i64 total_size = a->size + b->size;
  i8* concat = calloc(total_size + 1, sizeof(i8));
  strcat((char*) concat, (char*) a->ptr);
  strcat((char*) concat, (char*) b->ptr);

  struct string* new_string = malloc(sizeof(struct string));
  new_string->ptr = concat;
  new_string->size = total_size;

  return new_string;
}

i8 fpP8internalP9operators8get_char(struct string* string, i64 idx) {
  ensure_bounds_or_panic("String", string->size, idx);
  return string->ptr[idx];
}

i8* alloc_array(i64 size, i64 elements) {
  i8* ptr = calloc(elements, size);

  struct array* array_ptr = malloc(sizeof(struct array));
  array_ptr->ptr = ptr;
  array_ptr->size = elements;

  return (i8*) array_ptr;
}

i8* alloc_object(i64 size) {
  i8* ptr = malloc(size + 1);
  ptr[0] = 20; //Stupid testing value. TODO: Replace this with a real object type identifier.
  return ptr + 1;
}

void* array_idx_at(struct array* array, i64 elem_size, i64 idx) {
  ensure_bounds_or_panic("<array>", array->size, idx);
  return array->ptr + (elem_size * idx);
}

void fp5print(struct string* str) {
  printf("%s", str->ptr);
}


extern uint8_t __LLVM_StackMaps[] __attribute__ ((section(".llvm_stackmaps")));
statepoint_table_t* table;
bool table_built = false;

void fP1a2gc() {
    i8* esp = __builtin_frame_address(0);
    esp += sizeof(void*);
    int64_t ret = *(int64_t*) esp;
    esp += sizeof(void*);

    if (!table_built) {
        printf("Building table!\n");

        table = generate_table((void*) &__LLVM_StackMaps, 0.5);

        printf("GC is invoked!\nprinting the table...\n");
        print_table(stdout, table, true);
        printf("\n");
        table_built = true;
    }

    frame_info_t* frame = lookup_return_address(table, ret);

    while (frame) {
        printf("Frame @ 0x%" PRIX64 "\n", ret);


        esp += frame->frameSize;
        ret = *(int64_t*) esp;
        esp += sizeof(void*);

        frame = lookup_return_address(table, (int64_t) ret);
    }

    printf("End of frames!\n\n");
}
