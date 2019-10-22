#include "clib.h"

struct string* gc_alloc_string(i8* string, i64 length) {
  i8* block = malloc(sizeof(i16) + sizeof(struct string) + (length + 1) * sizeof(i8));
  *((i16*) block) = 0; // String type is ALWAYS 0.

  struct string* string_ptr = (struct string*) (block + sizeof(i16));
  string_ptr->length = length;
  memcpy(string_ptr->payload, string, length * sizeof(i8));
  string_ptr->payload[length] = '\0';

  return string_ptr;
}

i8* gc_alloc_array(i64 element_size, i64 elements, i16 type) {
  i8* block = malloc(sizeof(i16) + sizeof(struct array) + sizeof(i8) * element_size * elements);
  *((i16*) block) = type;

  struct array* array_ptr = (struct array*) (block + sizeof(i16));
  array_ptr->element_size = element_size;
  array_ptr->length = elements;
  bzero(array_ptr->payload, elements * element_size * sizeof(i8));

  return (i8*) array_ptr;
}

i8* gc_alloc_object(i64 size, i16 type) {
  i8* ptr = malloc(size + sizeof(i16));
  *((i16*) ptr) = type;
  return ptr + sizeof(i16);
}

i8* gc_array_idx_at(struct array* array, i64 idx) {
  _ensure_bounds_or_panic("<array>", array->length, idx);
  return array->payload + (array->element_size * idx);
}