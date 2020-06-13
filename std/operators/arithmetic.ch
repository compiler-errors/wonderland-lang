extern fn internal_add(a: Int, b: Int) -> Int.
extern fn internal_sub(a: Int, b: Int) -> Int.
extern fn internal_mul(a: Int, b: Int) -> Int.
extern fn internal_div(a: Int, b: Int) -> Int.
extern fn internal_rem(a: Int, b: Int) -> Int.
extern fn internal_xor(a: Int, b: Int) -> Int.
extern fn internal_lshr(a: Int, b: Int) -> Int.
extern fn internal_neg(a: Int) -> Int.

extern fn internal_csub(a: Char, b: Char) -> Int.

extern fn internal_fadd(a: Float, b: Float) -> Float.
extern fn internal_fsub(a: Float, b: Float) -> Float.
extern fn internal_fmul(a: Float, b: Float) -> Float.
extern fn internal_fdiv(a: Float, b: Float) -> Float.
extern fn internal_fneg(a: Float) -> Float.

extern fn internal_string_add(a: String, b: String) -> String.

trait Add<_T> {
  type Result.
  fn add(self, other: _T) -> <Self as Add<_T>>::Result.
}

impl Add<Int> for Int {
  type Result = Int.

  fn add(self, other: Int) -> Int = {
    internal_add(self, other)
  }.
}

impl Add<Float> for Float {
  type Result = Float.

  fn add(self, other: Float) -> Float = {
    internal_fadd(self, other)
  }.
}

impl Add<String> for String {
  type Result = String.

  fn add(self, other: String) -> String = {
    internal_string_add(self, other)
  }.
}

trait Subtract<_T> {
  type Result.
  fn sub(self, other: _T) -> <Self as Subtract<_T>>::Result.
}

impl Subtract<Int> for Int {
  type Result = Int.

  fn sub(self, other: Int) -> Int = {
    self + (-other)
  }.
}

impl Subtract<Float> for Float {
  type Result = Float.

  fn sub(self, other: Float) -> Float = {
    self + (-other)
  }.
}

impl Subtract<Char> for Char {
  type Result = Int.

  fn sub(self, other: Char) -> Int = {
    internal_csub(self, other)
  }.
}

trait Multiply<_T> {
  type Result.
  fn mul(self, other: _T) -> <Self as Multiply<_T>>::Result.
}

impl Multiply<Int> for Int {
  type Result = Int.

  fn mul(self, other: Int) -> Int = {
    internal_mul(self, other)
  }.
}

impl Multiply<Float> for Float {
  type Result = Float.

  fn mul(self, other: Float) -> Float = {
    internal_fmul(self, other)
  }.
}

trait Divide<_T> {
  type Result.
  fn div(self, other: _T) -> <Self as Divide<_T>>::Result.
}

impl Divide<Int> for Int {
  type Result = Int.

  fn div(self, other: Int) -> Int = {
    internal_div(self, other)
  }.
}

impl Divide<Float> for Float {
  type Result = Float.

  fn div(self, other: Float) -> Float = {
    internal_fdiv(self, other)
  }.
}

trait Modulo<_T> {
  type Result.
  fn rem(self, other: _T) -> <Self as Modulo<_T>>::Result.
}

impl Modulo<Int> for Int {
  type Result = Int.

  fn rem(self, other: Int) -> Int = {
    internal_rem(self, other)
  }.
}

trait Negate {
  type Result.
  fn negate(self) -> <Self as Negate>::Result.
}

impl Negate for Int {
  type Result = Int.

  fn negate(self) -> Int = {
    internal_neg(self)
  }.
}

impl Negate for Float {
  type Result = Float.

  fn negate(self) -> Float = {
    internal_fneg(self)
  }.
}

fn min<_T>(a: _T, b: _T) -> _T where _T: Compare<_T> = {
  if a < b {
    a
  } else {
    b
  }
}.

fn max<_T>(a: _T, b: _T) -> _T where _T: Compare<_T> = {
  if a > b {
    a
  } else {
    b
  }
}.

fn abs<_T>(i: _T) -> _T where _T: Negate<::Result=_T> + Compare<_T>, Int: Into<_T> = {
  if i >= (0 as _T) {
    i
  } else {
    -i
  }
}.
