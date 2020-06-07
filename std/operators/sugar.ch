fn allocate_empty_array_internal<_T>(n: Int) -> [_T] = {
  impl "llvm" {
    let ty_size = instruction "ch_typesize" (_ :_T) -> Int.
    instruction "ch_typeid" (_ :[_T]) -> $ty_id. // This is an i16, so let's store into an instruction value...
    instruction "call" ("gc_alloc_array", ty_size, n, $ty_id) -> $arr.
    instruction "pointercast" ($arr, _ :[_T]) -> [_T]
  } else impl "looking_glass" {
    instruction "allocate_array_undefined" (n) -> [_T]
  }
}.

fn commalipses_impl<_T>(where_at: String) -> _T = {
    panic("TODO: implement commalipses at \(where_at)")
}.

fn assert_impl(b: Bool, where_at: String) = {
    if !b {
        panic:<()>("Assert failed at \(where_at)").
    }
}.
