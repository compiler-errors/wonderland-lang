#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

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

i8* alloc_array(i64 size, i64 elements) {
    i8* ptr = calloc(elements, size);

    struct array* array_ptr = malloc(sizeof(struct array));
    array_ptr->ptr = ptr;
    array_ptr->size = elements;

    return (i8*) array_ptr;
}

void fp5print(struct string* str) {
    printf("%s", str->ptr);
}
