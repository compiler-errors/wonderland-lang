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

impl Add<Float> for Float {
  type Result = Float.

  fn add(self, other: Float) -> Float {
      instruction "fadd" (self, other) -> Float
  }
}

impl Add<String> for String {
  type Result = String.

  fn add(self, other: String) -> String {
      impl "llvm" {
        add_string_internal(self, other)
      } else impl "looking_glass" {
        instruction "add_string" (self, other) -> String
      }
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

impl Subtract<Float> for Float {
  type Result = Float.

  fn sub(self, other: Float) -> Float {
      self + (-other)
  }
}

impl Subtract<Char> for Char {
  type Result = Int.

  fn sub(self, other: Char) -> Int {
      impl "llvm" {
        instruction "sub" (self, other) -> $tmp.
        instruction "sext" ($tmp, :Int) -> Int
      } else impl "looking_glass" {
        instruction "csub" (self, other) -> Int
      }
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

impl Multiply<Float> for Float {
  type Result = Float.

  fn mul(self, other: Float) -> Float {
      instruction "fmul" (self, other) -> Float
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

impl Divide<Float> for Float {
  type Result = Float.

  fn div(self, other: Float) -> Float {
      instruction "fdiv" (self, other) -> Float
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

impl Negate for Float {
  type Result = Float.

  fn negate(self) -> Float {
      instruction "fneg" (self) -> Float
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

fn abs<_T>(i: _T) -> _T where _T: Negate<::Result=_T> + Compare<_T>, Int: Into<_T> {
  if i >= (0 as _T) {
    i
  } else {
    -i
  }
}
