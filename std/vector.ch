use std::operators::allocate_empty_array_internal.
use std::iterator::resize_array_internal.

let default_size: Int = 8.

object Vector<_T> {
  array: [_T].
  size: Int.
}

impl<_T> for Vector<_T> {
  fn new() -> Vector<_T> {
    allocate Vector<_T> {
      array: allocate_empty_array_internal:<_T>(8),
      size: 0
    }
  }

  fn new_with_size_hint(n: Int) -> Vector<_T> {
    n = if n < 0 { 0 } else { n }.

    allocate Vector<_T> {
      array: allocate_empty_array_internal:<_T>(n),
      size: 0
    }
  }

  fn push(self, t: _T) {
    self:ensure_size(self:size + 1).
    self:array[self:size] = t.
    self:size = self:size + 1.
  }

  fn pop(self) -> Option<_T> {
    if self:size > 0 {
      self:size = self:size - 1.
      let elem = self:array[self:size].
      self:array[self:size] = instruction "ch_zeroed" (:_T) -> _T.
      //self:try_downsize().
      Option!Some(elem)
    } else {
      Option!None
    }
  }

  fn ensure_size(self, n: Int) {
    if self:array:len() >= n {
      return.
    }

    let new_size = self:array:len().
    while new_size < n {
      new_size = new_size * 2.
    }

    self:array = resize_array_internal:<_T>(self:array, new_size).
    assert self:array:len() >= n.
  }

  fn into_array(self) -> [_T] {
    if self:array:len() != self:size {
      resize_array_internal(self:array, self:size)
    } else {
      self:array
    }
  }
}

impl<_T> Iterable for Vector<_T> {
  type Iterator = VectorIterator<_T>.
  type Item = _T.

  fn iterator(self) -> VectorIterator<_T> {
      VectorIterator!Iterator { vector: self, idx: 0 }
  }
}

enum VectorIterator<_T> {
  Iterator {
    vector: Vector<_T>,
    idx: Int
  }.
}

impl<_T> Iterator for VectorIterator<_T> {
  type Item = _T.

  fn next(self) -> (Option<_T>, VectorIterator<_T>) {
      let VectorIterator!Iterator { vector, idx } = self.

      if idx >= vector:size {
        (Option!None, VectorIterator!Iterator { vector, idx })
      } else {
        (Option!Some(vector:array[idx]), VectorIterator!Iterator { vector, idx: idx + 1 })
      }
  }

  fn has_next(self) -> Bool {
    let VectorIterator!Iterator { vector, idx } = self.

    idx < vector:size
  }

  fn size_hint(self) -> Int {
    let VectorIterator!Iterator { vector, idx } = self.

    vector:size - idx
  }
}

impl<_T> Len for Vector<_T> {
    fn len(self) -> Int = self:size.
}

impl<_T> Deref for Vector<_T> {
  type Idx = Int.
  type Result = _T.

  fn deref(self, idx: Int) -> _T {
      if idx < 0 | idx >= self:len() {
          panic:<()>("Index \(idx) out of bounds. Size is \(self:len())!").
      }

      self:array[idx]
  }
}

impl<_T> DerefAssign for Vector<_T> {
  type Idx = Int.
  type Value = _T.

  fn deref_assign(self, idx: Int, value: _T) -> _T {
      if idx < 0 | idx >= self:len() {
          panic:<()>("Index \(idx) out of bounds. Size is \(self:len())!").
      }

      self:array[idx] = value.
      value
  }
}

impl<_T> Into<String> for Vector<_T> where _T: Into<String> {
  fn into(self) -> String {
      let s = "Vector[".
      let first = true.

      for i in self {
          if first {
              first = false.
          } else {
              s = s + ", ".
          }

          s = s + (i as String).
      }

      s + "]"
  }
}

impl<_I> FromIterator<_I> for Vector<_I> {
  fn from_iterator<_It>(it: _It) -> Vector<_I> where _It: Iterator<::Item=_I> {
      let v = Vector:new_with_size_hint(it:size_hint()).

      for i in it {
        v:push(i).
      }

      v
  }
}
