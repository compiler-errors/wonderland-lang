trait Len {
  fn len(self) -> Int.
}

impl<_T> Len for [_T] {
  fn len(self) -> Int {
      impl "llvm" {
        instruction "getelementptr" (self, 0, 0) -> $len.
        instruction "load" ($len) -> Int
      } else impl "looking_glass" {
        instruction "array_len" (self) -> Int
      }
  }
}

impl Len for String {
  fn len(self) -> Int {
      impl "llvm" {
        instruction "getelementptr" (self, 0, 0) -> $len.
        instruction "load" ($len) -> Int
      } else impl "looking_glass" {
        instruction "string_len" (self) -> Int
      }
  }
}

trait Hash {
  fn hash(self) -> Int.
}

impl Hash for Int {
  fn hash(self) -> Int {
    self * -7046029254386353131
  }
}

impl Hash for String {
  fn hash(self) -> Int {
    let h = 525201411107845655.

    for c in self {
      let c_as_i = impl "llvm" {
        instruction "zext" (c, _ :Int) -> Int
      } else impl "looking_glass" {
        instruction "reinterpret" (c, _ :Int) -> Int
      }.

      h = instruction "xor" (h, c_as_i) -> Int.
      h = h * 6616326155283851669.
      h = instruction "xor" (h, instruction "lshr" (c_as_i, 47) -> Int) -> Int.
    }

    h
  }
}

impl Hash for Char {
  fn hash(self) -> Int {
    let c_as_i = impl "llvm" {
      instruction "zext" (self, _ :Int) -> Int
    } else impl "looking_glass" {
      instruction "reinterpret" (self, _ :Int) -> Int
    }.
    
    c_as_i:hash()
  }
}

impl Hash for Bool {
  fn hash(self) -> Int = if self { 0 } else { 1 }.
}
