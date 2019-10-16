#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

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

i1 fpP9operatorsP8internal6eq_int(i64 a, i64 b) {
    return a == b;
}

i1 fpP9operatorsP8internal6gt_int(i64 a, i64 b) {
    return a > b;
}

i64 fpP9operatorsP8internal7add_int(i64 a, i64 b) {
    return a + b;
}

i64 fpP9operatorsP8internal7sub_int(i64 a, i64 b) {
    return a - b;
}

i64 fpP9operatorsP8internal7mul_int(i64 a, i64 b) {
    return a * b;
}

i64 fpP9operatorsP8internal7div_int(i64 a, i64 b) {
    return a / b;
}

i64 fpP9operatorsP8internal7mod_int(i64 a, i64 b) {
    return a % b;
}

i8* alloc_string(i8* string, i64 size) {
    I need to copy the string here, since I will deref it later.

    struct string* string_ptr = malloc(sizeof(struct string));
    string_ptr->ptr = string;
    string_ptr->size = size;

    return (i8*) string_ptr;
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