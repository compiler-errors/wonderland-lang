#include "clib.h"
#include "statepoint.h"

#define GC_CALLBACK i1(*callback)(i8** ptr)
const i16 MARK_FLAG = 1 << 15;

// ----- ----- ----- ----- ----- ----- GC mark shit ----- ----- ----- ----- ----- ----- //

i16 gc_get_type(i8* val) {
  val -= sizeof(i16);
  i16 mark = *(i16*) val;
  return mark & ~MARK_FLAG;
}

i1 gc_get_mark(i8* val) {
  val -= sizeof(i16);
  i16 mark = *(i16*) val;
  return (mark & MARK_FLAG) > 0;
}

void gc_set_mark(i8* val, i1 mark) {
  val -= sizeof(i16);
  if (mark) {
    (*(i16*) val) |= MARK_FLAG;
  } else {
    (*(i16*) val) &= ~MARK_FLAG;
  }
}

i1 gc_mark(i8** ptr) {
  i8* val = *ptr;
  if (val == NULL) {
    DEBUG_PRINTF("Tried to mark NULL\n");
    return false;
  }

  // Is it already marked?
  if (gc_get_mark(val)) {
    DEBUG_PRINTF("Marked %p: Is marked.\n", val);
    return false;
  } else {
    gc_set_mark(val, 1);
    DEBUG_PRINTF("Marking %p.\n", val);
    return true;
  }
}

// ----- ----- ----- ----- ----- ----- GC remap shit ----- ----- ----- ----- ----- ----- //

struct gc_remap_entry {
    i8* from;
    i8* to;
    struct gc_remap_entry* next;
}* GC_REMAP_TABLE;

void gc_remap_free(void) {
    struct gc_remap_entry* entry = GC_REMAP_TABLE;

    while (entry) {
        struct gc_remap_entry* next = entry->next;
        free(entry);
        entry = next;
    }

    GC_REMAP_TABLE = NULL;
}

i8* gc_remap_block(i8* ptr) {
    struct gc_remap_entry* entry = GC_REMAP_TABLE;

    while (entry) {
        if (entry->from == ptr) {
            DEBUG_PRINTF("REMAP: %p -> %p\n", entry->from, entry->to);

            if ((i64)entry->to < 100) {
                fprintf(stderr, "Bad map.\n");
                exit(-1);
            }

            return entry->to;
        }

        entry = entry->next;
    }

    exit(-1);
}

i8* gc_remap_object(i8* ptr) {
    if (GC_REMAP_TABLE == NULL) {
        return ptr;
    }

    return gc_remap_block(ptr - sizeof(i32) - sizeof(i16)) + sizeof(i32) + sizeof(i16);
}

void gc_remap_insert_block(i8* from, i8* to) {
    struct gc_remap_entry* next = malloc(sizeof(struct gc_remap_entry));
    next->from = from;
    next->to = to;
    next->next = GC_REMAP_TABLE;
    GC_REMAP_TABLE = next;
}

i1 gc_remap_and_unmark(i8** ptr) {
  if (*ptr == NULL) {
    DEBUG_PRINTF("Tried to remap NULL\n");
    return false;
  }

  i8* to = *ptr = gc_remap_object(*ptr);

  // Is it marked? If so, then we haven't yet visited the children. Unmark and visit.
  if (gc_get_mark(to)) {
    gc_set_mark(to, 0);
    DEBUG_PRINTF("Remapping %p's children.\n", to);
    return true;
  } else {
    DEBUG_PRINTF("Remapped %p: Children have been remapped.\n", to);
    return false;
  }
}

// ----- ----- ----- ----- ----- ----- GC visit shit ----- ----- ----- ----- ----- ----- //

// {ptr} is a pointer to the value, not the value itself. Deref it, THEN cast to a (struct string*), etc.
// NOTE: This function is auto-generated by cheshire itself.
void gc_visit(i8* ptr, i16 ty, GC_CALLBACK);

void gc_visit_array(struct array** array_ptr, i16 element_ty, GC_CALLBACK) {
  DEBUG_PRINTF("Visiting array %p in %p\n", *array_ptr, array_ptr);

  if (*array_ptr == NULL) {
    return;
  }

  if (!callback((i8**) array_ptr)) {
    return;
  }

  // _MUST_ read the array after calling the callback. We might remap it in the callback!
  struct array* array = *array_ptr;
  // TODO: lmao use a better iteration type.
  for (int i = 0; i < array->length; i++) {
    DEBUG_PRINTF("Element %d, TYPE=%"PRId16"\n", i, element_ty);
    gc_visit(array->payload + i * array->element_size, element_ty, callback);
  }
}

void gc_visit_closure(i8** callback_ptr, GC_CALLBACK) {
    DEBUG_PRINTF("Visiting closure %p at %p\n", *callback_ptr, callback_ptr);

    if (*callback_ptr == NULL) {
        return;
    }

    i8* moved_callback = gc_remap_object(*callback_ptr);
    gc_visit((i8*) callback_ptr, gc_get_type(moved_callback), callback);
}

// ----- ----- ----- ----- ----- ----- GC allocation shit ----- ----- ----- ----- ----- ----- //

i8* GC_BEGIN = NULL;
i8* GC_END = NULL;
i8* GC_LIMIT = NULL;

const i64 GC_BEGIN_SIZE = 800; /* 800 bytes. Why? Idfk. */
const i64 GC_OOM_LIMIT = 100 * 1024 * 1024; /* 100 MB. */
const i64 GC_BLOCK_LIMIT = 0xFFFFFFFF;

void* gc_alloc_block(i64 size, i16 type, i8* cheshire_stack_root) {
    gc_remap_free();

    // We want to keep our blocks under 32 bits long...
    // TODO: We might want to unify this into one nice type/constant set so we don't forget to update this.
    if (size >= GC_BLOCK_LIMIT) {
        fprintf(stderr, "OOM: Not allowed to allocate a block of size %"PRId64" bytes!\n", size);
        exit(1);
    }

    // Alloc a tiny bit of data for the flag, and the size
    size += sizeof(i32) + sizeof(i16);

    // If we have a block, but we don't have any space, then garbage collect.
    if (GC_BEGIN != NULL && GC_LIMIT - GC_END < size) {
        gc(cheshire_stack_root);
    }

    // Otherwise, let's scale the pool size to fit our new allocation.
    // We might OOM in the process, but too bad. That's not necessarily our fault.
    while (GC_BEGIN == NULL || GC_LIMIT - GC_END < size) {
        i64 current_pool_size = GC_LIMIT - GC_BEGIN;
        i64 current_used_size = GC_END - GC_LIMIT;
        i64 new_pool_size = (current_pool_size == 0) ? GC_BEGIN_SIZE : (current_pool_size * 2);

        if (new_pool_size > GC_OOM_LIMIT) {
            fprintf(stderr, "OOM: Not allowed to allocate a pool of size %"PRId64" bytes!\n", new_pool_size);
            exit(1);
        }

        GC_BEGIN = realloc(GC_BEGIN, new_pool_size);
        if (GC_BEGIN == NULL) {
            fprintf(stderr, "OOM: Can't seem to allocate a pool of size %"PRId64" bytes!\n", new_pool_size);
            exit(1);
        }

        GC_END = GC_BEGIN + current_used_size;
        GC_LIMIT = GC_BEGIN + new_pool_size;
    }

    // If we made it here, we should have sufficient space!
    i8* block = GC_END;
    bzero(block, size);
    GC_END += size;

    // Insert size, so we can walk the block.
    *(i32*) block = (i32) size;
    block += sizeof(i32);

    // Insert type, so we can garbage collect properly.
    *(i16*) block = (i16) type;
    block += sizeof(i16);

    DEBUG_PRINTF("Allocated size %"PRId64" ID=%"PRId16" @ %p\n", size, type, block);

    return block;
}

// ----- ----- ----- ----- ----- ----- GC walking shit ----- ----- ----- ----- ----- ----- //

extern uint8_t __LLVM_StackMaps[] __attribute__ ((section(".llvm_stackmaps")));

statepoint_table_t* table;
bool table_built = false;

void gc_verify_trivial_derive(i8* cheshire_stack_root, pointer_slot_t* slots, pointer_slot_t ptrSlot) {
    if (ptrSlot.kind >= 0) {
        i8** stack_ptr = (i8**) (cheshire_stack_root + ptrSlot.offset);

        pointer_slot_t baseSlot = slots[ptrSlot.kind];
        i8** base_stack_ptr = (i8**) (cheshire_stack_root + baseSlot.offset);

        DEBUG_PRINTF("Pointer %p derives from pointer %p\n", *stack_ptr, *base_stack_ptr);

        if (*stack_ptr != *base_stack_ptr) {
            fprintf(stderr, "Pointer %p derives from pointer %p\n", *stack_ptr, *base_stack_ptr);
            fprintf(stderr, "ICE: Non-trivial derive. Not sure how this happened... at all!\n");
            exit(-1);
        }

        gc_verify_trivial_derive(cheshire_stack_root, slots, baseSlot);
    }
}

void gc_walk(i8* cheshire_stack_root, i1 verify_derives, GC_CALLBACK) {
    cheshire_stack_root += sizeof(void*);
    int64_t ret = *(int64_t*) cheshire_stack_root;
    cheshire_stack_root += sizeof(void*);

    if (!table_built) {
        DEBUG_PRINTF("Building table!\n");

        table = generate_table((void*) &__LLVM_StackMaps, 0.5);

        //DEBUG_PRINTF("GC is invoked");
        //print_table(stdout, table, true);
        //DEBUG_PRINTF("\n");
        table_built = true;
    }

    frame_info_t* frame = lookup_return_address(table, ret);

    if (frame == NULL) {
        // TODO: Panic, first frame should never be NULL.
    }

    while (frame) {
        DEBUG_PRINTF("Frame @ 0x%" PRIX64 " has %d slots\n", ret, frame->numSlots);

        for (int i = 0; i < frame->numSlots; i++) {
            pointer_slot_t ptrSlot = frame->slots[i];
            i8** stack_ptr = (i8**) (cheshire_stack_root + ptrSlot.offset);

            if (verify_derives) {
                gc_verify_trivial_derive(cheshire_stack_root, frame->slots, ptrSlot);
            }

            i16 type = gc_get_type(gc_remap_object(*stack_ptr));

            DEBUG_PRINTF("Visiting %p (type = %"PRId16")\n", *stack_ptr, type);
            gc_visit((i8*) stack_ptr, type, callback);
        }

        cheshire_stack_root += frame->frameSize;
        ret = *(int64_t*) cheshire_stack_root;
        cheshire_stack_root += sizeof(void*);

        frame = lookup_return_address(table, (int64_t) ret);
    }

    DEBUG_PRINTF("End of frames!\n");
}

NOINLINE void gc(i8* cheshire_stack_root) {
    gc_remap_free();

    // Mark
    DEBUG_PRINTF(" -- MARK --\n");
    gc_walk(cheshire_stack_root, true, gc_mark);

    // Compact
    DEBUG_PRINTF(" -- COMPACT --\n");
    DEBUG_PRINTF("Mem pool begins at %"PRId64" long.\n", (i64) (GC_END - GC_BEGIN));
    i8* from = GC_BEGIN;
    i8* to = GC_BEGIN;

    while (from < GC_END) {
        DEBUG_PRINTF("At block %p\n", from);
        i32 block_size = *(i32*) from;
        i16 id = gc_get_type(from + sizeof(i32) + sizeof(i16));
        i16 marked = gc_get_mark(from + sizeof(i32) + sizeof(i16));

        if (marked) {
            if (from != to) {
                memmove(to, from, block_size);
            }

            DEBUG_PRINTF("Kept something... ID=%"PRId16"\n", id);
            gc_remap_insert_block(from, to);
            to += block_size;
        } else {
            DEBUG_PRINTF("Reclaimed something... ID=%"PRId16"", id);
            if (id == 0) {
                struct string* str = (struct string*) (from + sizeof(i32) + sizeof(i16));
                DEBUG_PRINTF(" STRING='%s'\n", str->payload);
            } else {
                DEBUG_PRINTF("\n");
            }

            gc_remap_insert_block(from, NULL);
        }

        from += block_size;
    }

    GC_END = to;
    DEBUG_PRINTF("Mem pool is now %"PRId64" long. End = %p\n", (i64) (GC_END - GC_BEGIN), GC_END);

    // Remap the pointers
    DEBUG_PRINTF(" -- UNMARK-REMAP --\n");
    gc_walk(cheshire_stack_root, false, gc_remap_and_unmark);

    memset(GC_END, 0xCC, GC_LIMIT - GC_END);
}

NOINLINE void fp2gc(void) {
  gc(__builtin_frame_address(0));
}