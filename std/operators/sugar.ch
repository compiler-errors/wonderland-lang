trait Call<_Args> {
  type Return.
  fn call(self, args: _Args) -> <Self as Call<_Args>>::Return.
}

trait AllocateArray {
  fn allocate_array(n: Int) -> [Self].
}

impl<_T> AllocateArray for _T where _T: Default {
  fn allocate_array(n: Int) -> [_T] {
      let a = allocate_empty_array_internal:<_T>(n).

      for i in Range!Finite(0, n) {
        a[i] = <_T>:default().
      }

      a
  }
}

fn allocate_empty_array_internal<_T>(n: Int) -> [_T] {
  impl "llvm" {
    let ty_size = instruction "ch_typesize" (_ :_T) -> Int.
    instruction "ch_typeid" (_ :[_T]) -> $ty_id. // This is an i16, so let's store into an instruction value...
    instruction "call" ("gc_alloc_array", ty_size, n, $ty_id) -> $arr.
    instruction "pointercast" ($arr, _ :[_T]) -> [_T]
  } else impl "looking_glass" {
    instruction "allocate_array_undefined" (n) -> [_T]
  }
}

trait Into<_T> {
  fn into(self) -> _T.
}

impl<_T> Into<_T> for _T {
  fn into(self) -> _T = self.
}

impl Into<Float> for Int {
  fn into(self) -> Float {
    impl "llvm" {
      instruction "sitofp" (self) -> Float
    } else impl "looking_glass" {
      instruction "int_to_float" (self) -> Float
    }
  }
}

fn commalipses_impl<_T>(where_at: String) -> _T {
    panic("TODO: implement commalipses at \(where_at)")
}

fn assert_impl(b: Bool, where_at: String) {
    if !b {
        panic:<()>("Assert failed at \(where_at)").
    }
}
