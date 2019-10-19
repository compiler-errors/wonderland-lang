#include "clib.h"

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
  _ensure_bounds_or_panic("<array>", array->size, idx);
  return array->ptr + (elem_size * idx);
}