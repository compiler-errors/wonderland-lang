trait Deref {
  type Idx.
  type Result.

  fn deref(self, idx: <Self as Deref>::Idx) -> <Self as Deref>::Result.
}

impl<_T> Deref for [_T] {
  type Idx = Int.
  type Result = _T.

  fn deref(self, idx: Int) -> _T {
      if idx >= self:len() {
          panic:<()>("Index out of bounds for \(type_string:<Self>())... length = \(self:len()), index = \(idx).").
      }

      impl "llvm" {
        instruction "getelementptr" (self, 0, 2, idx) -> $ptr.
        instruction "load" ($ptr) -> _T
      } else impl "looking_glass" {
        instruction "array_deref" (self, idx) -> _T
      }
  }
}

impl Deref for String {
  type Idx = Int.
  type Result = Char.

  fn deref(self, idx: Int) -> Char {
    impl "llvm" {
      instruction "getelementptr" (self, 0, 1, idx) -> $ptr.
      instruction "load" ($ptr) -> Char
    } else impl "looking_glass" {
      instruction "string_deref" (self, idx) -> Char
    }
  }
}

trait DerefAssign {
  type Idx.
  type Value.

  fn deref_assign(
      self,
      idx: <Self as DerefAssign>::Idx,
      value: <Self as DerefAssign>::Value) -> <Self as DerefAssign>::Value.
}

impl<_T> DerefAssign for [_T] {
  type Idx = Int.
  type Value = _T.

  fn deref_assign(self, idx: Int, value: _T) -> _T {
      if idx >= self:len() {
          panic:<()>("Index out of bounds for String... length = \(self:len()), index = \(idx).").
      }

      impl "llvm" {
        instruction "getelementptr" (self, 0, 2, idx) -> $ptr.
        instruction "store" ($ptr, value) -> ().
      } else impl "looking_glass" {
        instruction "array_store" (self, idx, value) -> ().
      }

      value
  }
}
