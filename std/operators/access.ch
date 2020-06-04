trait Deref<_Idx> {
  type Result.

  fn deref(self, idx: _Idx) -> <Self as Deref<_Idx>>::Result.
}

impl<_T> Deref<Int> for [_T] {
  type Result = _T.

  fn deref(self, idx: Int) -> _T {
      if idx < 0 |? idx >= self:len() {
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

impl<_T> Deref<RangeIterator> for [_T] {
  type Result = [_T].

  fn deref(self, idx: RangeIterator) -> [_T] {
    let len = self:len().

    let (start, end) = match idx {
      RangeIterator!Finite(a, b) => {
        if a > b {
          panic:<()>("Start index of array slice is greater than end. Start = \(a), End = \(b).").
        }

        (a, b)
      },
      RangeIterator!Infinite(a) => (a, self:len()),
    }.

    if 0 > start {
      panic:<()>("Start index of array slice is less than 0. Start = \(start)").
    }

    if start > len {
      panic:<()>("Start index of array slice is out of bounds. Start = \(start), length = \(len).").
    }

    if end > len {
      panic:<()>("End index of array slice is out of bounds. End = \(start), length = \(len).").
    }

    instruction "array_slice" (self, start, end) -> [_T]
  }
}

impl Deref<Int> for String {
  type Result = Char.

  fn deref(self, idx: Int) -> Char {
    if idx < 0 |? idx >= self:len() {
      panic:<()>("Index out of bounds for String... length = \(self:len()), index = \(idx).").
    }

    impl "llvm" {
      instruction "getelementptr" (self, 0, 1, idx) -> $ptr.
      instruction "load" ($ptr) -> Char
    } else impl "looking_glass" {
      instruction "string_deref" (self, idx) -> Char
    }
  }
}

trait DerefAssign<_Idx> {
  type Value.

  fn deref_assign(
      self,
      idx: _Idx,
      value: <Self as DerefAssign<_Idx>>::Value) -> <Self as DerefAssign<_Idx>>::Value.
}

impl<_T> DerefAssign<Int> for [_T] {
  type Value = _T.

  fn deref_assign(self, idx: Int, value: _T) -> _T {
      if idx < 0 |? idx >= self:len() {
          panic:<()>("Index out of bounds for \(type_string:<Self>())... length = \(self:len()), index = \(idx).").
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
