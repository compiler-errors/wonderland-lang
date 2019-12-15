#include "clib.h"

void fp5print(struct string* str) {
  printf("%s", str->payload);
}

i1 fpP8internalP9operators6eq_int(i64 a, i64 b) {
  return a == b;
}

i1 fpP8internalP9operators6gt_int(i64 a, i64 b) {
  // SIGNED comparison, please.
  return ((int64_t) a) > ((int64_t) b);
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

NOINLINE struct string* fpP8internalP16transmute_string15int_into_string(i64 i) {
  i64 length = snprintf(NULL, 0, "%"PRId64"", i);

  i64 block_size = (i64) (sizeof(struct string) + (length + 1) * sizeof(i8));
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));
  gc_remap_free();

  string_ptr->length = length;
  snprintf((char*) string_ptr->payload, length + 1, "%"PRId64"", i);

  return string_ptr;
}

NOINLINE struct string* fpP8internalP16transmute_string16char_into_string(i8 c) {
  i64 block_size = (i64) (sizeof(struct string) + 2 * sizeof(i8));
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));
  gc_remap_free();

  string_ptr->length = 1;
  string_ptr->payload[0] = c;
  string_ptr->payload[1] = '\0';

  return string_ptr;
}

NOINLINE struct string* fpP8internalP9operators10add_string(struct string* a,
                                                   struct string* b) {
  i64 length = strlen((char*) a->payload) + strlen((char*) b->payload);

  i64 block_size = (i64) (sizeof(struct string) + (length + 1) * sizeof(i8));

  gc_mark((i8**) &a);
  gc_mark((i8**) &b);
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));
  gc_remap_and_unmark((i8**) &a);
  gc_remap_and_unmark((i8**) &b);
  gc_remap_free();

  string_ptr->length = length;
  snprintf((char*) string_ptr->payload, length + 1, "%s%s", a->payload, b->payload);

  return string_ptr;
}

i8 fpP8internalP9operators8get_char(struct string* string, i64 idx) {
  _ensure_bounds_or_panic("String", string->length, idx);
  return string->payload[idx];
}

i64 fpP8internalP9operators10len_string(struct string* string) {
    return string->length;
}

i1 fpP8internalP9operators9eq_string(struct string* a, struct string* b) {
    if (a->length != b->length) {
        return false;
    }

    return strncmp((char*) a->payload, (char*) b->payload, a->length) == 0;
}