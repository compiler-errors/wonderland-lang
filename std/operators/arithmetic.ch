trait Add<_T> {
  type Result.
  fn add(self, other: _T) -> <Self as Add<_T>>::Result.
}

impl Add<Int> for Int {
  type Result = Int.

  fn add(self, other: Int) -> Int {
      instruction "add" (self, other) -> Int
  }
}

impl Add<String> for String {
  type Result = String.

  fn add(self, other: String) -> String {
      add_string_internal(self, other)
  }
}

trait Subtract<_T> {
  type Result.
  fn sub(self, other: _T) -> <Self as Subtract<_T>>::Result.
}

impl Subtract<Int> for Int {
  type Result = Int.

  fn sub(self, other: Int) -> Int {
      self + (-other)
  }
}

impl Subtract<Char> for Char {
  type Result = Int.

  fn sub(self, other: Char) -> Int {
      instruction "sub" (self, other) -> $tmp.
      instruction "sext" ($tmp, :Int) -> Int
  }
}

trait Multiply<_T> {
  type Result.
  fn mul(self, other: _T) -> <Self as Multiply<_T>>::Result.
}

impl Multiply<Int> for Int {
  type Result = Int.

  fn mul(self, other: Int) -> Int {
      instruction "mul" (self, other) -> Int
  }
}

trait Divide<_T> {
  type Result.
  fn div(self, other: _T) -> <Self as Divide<_T>>::Result.
}

impl Divide<Int> for Int {
  type Result = Int.

  fn div(self, other: Int) -> Int {
      instruction "sdiv" (self, other) -> Int
  }
}

trait Modulo<_T> {
  type Result.
  fn rem(self, other: _T) -> <Self as Modulo<_T>>::Result.
}

impl Modulo<Int> for Int {
  type Result = Int.

  fn rem(self, other: Int) -> Int {
      instruction "srem" (self, other) -> Int
  }
}

trait Negate {
  type Result.
  fn negate(self) -> <Self as Negate>::Result.
}

impl Negate for Int {
  type Result = Int.

  fn negate(self) -> Int {
      instruction "neg" (self) -> Int
  }
}

fn min<_T>(a: _T, b: _T) -> _T where _T: Compare<_T> {
  if a < b {
      a
  } else {
      b
  }
}

fn max<_T>(a: _T, b: _T) -> _T where _T: Compare<_T> {
  if a > b {
      a
  } else {
      b
  }
}

fn abs(i: Int) -> Int {
  if i >= 0 {
    i
  } else {
    -i
  }
}
