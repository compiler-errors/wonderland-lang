extern fn internal_gt(a: Int, b: Int) -> Bool.
extern fn internal_eq(a: Int, b: Int) -> Bool.

extern fn internal_fgt(a: Float, b: Float) -> Bool.
extern fn internal_feq(a: Float, b: Float) -> Bool.

extern fn internal_string_eq(a: String, b: String) -> Bool.

trait PartialCompare<_T> {
  fn compare(self, other: _T) -> Int.
}

impl PartialCompare<Int> for Int {
  fn compare(self, other: Int) -> Int = {
    self - other
  }.
}

impl PartialCompare<Float> for Float {
  fn compare(self, other: Float) -> Int = {
    if internal_feq(self, other) {
      0
    } else if internal_fgt(self, other) {
      1
    } else {
      -1
    }
  }.
}

impl PartialCompare<Char> for Char {
  fn compare(self, other: Char) -> Int = {
    self - other
  }.
}

trait Compare<_T> {
  fn gt(self, other: _T) -> Bool.
  fn lt(self, other: _T) -> Bool.
  fn ge(self, other: _T) -> Bool.
  fn le(self, other: _T) -> Bool.
}

impl<_S, _T> Compare<_T> for _S where _S: PartialCompare<_T> {
  fn gt(self, other: _T) -> Bool = {
    let res = self:compare(other).
    internal_gt(res, 0)
  }.

  fn lt(self, other: _T) -> Bool = {
    let res = self:compare(other).
    internal_gt(0, res)
  }.

  fn ge(self, other: _T) -> Bool = {
    !(self < other)
  }.

  fn le(self, other: _T) -> Bool = {
    !(self > other)
  }.
}

trait Equals<_T> {
  fn eq(self, other: _T) -> Bool.
  fn ne(self, other: _T) -> Bool.
}

impl<_S, _T> Equals<_T> for _S where _S: PartialCompare<_T> {
  fn eq(self, other: _T) -> Bool = {
    let res = self:compare(other).
    internal_eq(res, 0)
  }.

  fn ne(self, other: _T) -> Bool = {
    !(self == other)
  }.
}

impl Equals<String> for String {
  fn eq(self, other: String) -> Bool = {
    internal_string_eq(self, other)
  }.

  fn ne(self, other: String) -> Bool = {
    !internal_string_eq(self, other)
  }.
}

trait And<_T> {
  type Result.
  fn and(self, other: _T) -> <Self as And<_T>>::Result.
}

impl And<Bool> for Bool {
  type Result = Bool.

  fn and(self, other: Bool) -> Bool = {
    self &? other
  }.
}

trait Or<_T> {
  type Result.
  fn or(self, other: _T) -> <Self as Or<_T>>::Result.
}

impl Or<Bool> for Bool {
  type Result = Bool.

  fn or(self, other: Bool) -> Bool = {
    self |? other
  }.
}

trait Not {
  type Result.
  fn not(self) -> <Self as Not>::Result.
}

impl Not for Bool {
  type Result = Bool.

  fn not(self) -> Bool = {
    if self {
        false
    } else {
        true
    }
  }.
}
