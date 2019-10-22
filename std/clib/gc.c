#include "clib.h"
#include "statepoint.h"

// {ptr} is a pointer to the VALUE, not the value itself. Deref it, THEN cast to a (struct string*), etc.
// NOTE: This function is auto-generated by cheshire itself.
void gc_visit(i8* ptr, i16 ty);

i1 gc_mark(i8* val) {
  val -= sizeof(i16);
  i16 mark = *val;
  // Is it already marked?
  if (MARK_FLAG & mark) {
    return true;
  } else {
    *val = mark | MARK_FLAG;
    return false;
  }
}

void gc_visit_array(struct array* array, i16 element_ty) {
  // TODO: lmao use a better iteration type.
  for (int i = 0; i < array->length; i++) {
    gc_visit(array->payload + i * array->element_size, element_ty);
  }
}

// ----- ----- ----- ----- ----- ----- GC walking shit ----- ----- ----- ----- ----- ----- //

extern uint8_t __LLVM_StackMaps[] __attribute__ ((section(".llvm_stackmaps")));

statepoint_table_t* table;
bool table_built = false;

void gc(void) {
    i8* esp = __builtin_frame_address(2);
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
        printf("Frame @ 0x%" PRIX64 " has %d slots\n", ret, frame->numSlots);

        for (int i = 0; i < frame->numSlots; i++) {

        }

        esp += frame->frameSize;
        ret = *(int64_t*) esp;
        esp += sizeof(void*);

        frame = lookup_return_address(table, (int64_t) ret);
    }

    printf("End of frames!\n\n");
}

void fP1a2gc_0(void) {
  // TODO: Never inline me.
  gc(); // One stackframe above cheshire now, gc() will be 2 stackframes above cheshire.
}

void fP1a2gc(void) {
  // TODO: Never inline me.
  fP1a2gc_0(); // I do this because I need to be 2 stack frames above cheshire.
}