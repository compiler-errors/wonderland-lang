trait Len {
  fn len(self) -> Int.
}

impl<_T> Len for [_T] {
  fn len(self) -> Int {
      instruction "getelementptr" (self, 0, 0) -> $len.
      instruction "load" ($len) -> Int
  }
}

impl Len for String {
  fn len(self) -> Int {
      instruction "getelementptr" (self, 0, 0) -> $len.
      instruction "load" ($len) -> Int
  }
}

trait Hash {
  fn hash(self) -> Int.
}

impl Hash for Int {
  fn hash(self) -> Int {
    self * 11400714819323198485
  }
}

impl Hash for String {
  fn hash(self) -> Int {
    let h = 525201411107845655.

    for c in self {
      let c_as_i = impl "llvm" {
        instruction "zext" (c, :Int) -> Int
      } else impl "looking_glass" {
        instruction "reinterpret" (c, :Int) -> Int
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
      instruction "zext" (self, :Int) -> Int
    } else impl "looking_glass" {
      instruction "reinterpret" (self, :Int) -> Int
    }.
    
    c_as_i:hash()
  }
}

impl Hash for Bool {
  fn hash(self) -> Int = if self { 0 } else { 1 }.
}
