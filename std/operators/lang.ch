// Transmute all basic types to string
extern fn internal_itos(i: Int) -> String.
extern fn internal_ftos(f: Float) -> String.
extern fn internal_ctos(c: Char) -> String.

// ctoi is basically a no-op in VS, and itof is just widening
extern fn internal_ctoi(c: Char) -> Int.
extern fn internal_itof(c: Int) -> Float.

// Allocate an array full of undefined values
extern fn internal_alloc_empty_array<_T>(n: Int) -> [_T].

// Length functions
extern fn internal_array_len<_T>(a: [_T]) -> Int.
extern fn internal_string_len(s: String) -> Int.

trait Len {
  fn len(self) -> Int.
}

impl<_T> Len for [_T] {
  fn len(self) -> Int = {
    internal_array_len(self)
  }.
}

impl Len for String {
  fn len(self) -> Int = {
    internal_string_len(self)
  }.
}

trait Hash {
  fn hash(self) -> Int.
}

impl Hash for Int {
  fn hash(self) -> Int = {
    self * -7046029254386353131
  }.
}

impl Hash for String {
  fn hash(self) -> Int = {
    let h = 525201411107845655.

    for c in self {
      let c_as_i = internal_ctoi(c).

      h = internal_xor(h, c_as_i).
      h = h * 6616326155283851669.
      h = internal_xor(h, internal_lshr(c_as_i, 47)).
    }

    h
  }.
}

impl Hash for Char {
  fn hash(self) -> Int = {
    let c_as_i = internal_ctoi(self).

    c_as_i:hash()
  }.
}

impl Hash for Bool {
  fn hash(self) -> Int = if self { 0 } else { 1 }.
}

trait Call<_Args> {
  type Return.
  fn call(self, args: _Args) -> <Self as Call<_Args>>::Return.
}

trait AllocateArray {
  fn allocate_array(n: Int) -> [Self].
}

impl<_T> AllocateArray for _T where _T: Default {
  fn allocate_array(n: Int) -> [_T] = {
    let a = internal_alloc_empty_array:<_T>(n).

    for i in 0..n {
      a[i] = <_T>:default().
    }

    a
  }.
}

trait Into<_T> {
  fn into(self) -> _T.
}

impl<_T> Into<_T> for _T {
  fn into(self) -> _T = self.
}

impl Into<Float> for Int {
  fn into(self) -> Float = {
    internal_itof(self)
  }.
}

impl Into<String> for Int {
  fn into(self) -> String = {
    internal_itos(self)
  }.
}

impl Into<String> for Float {
  fn into(self) -> String = {
    internal_ftos(self)
  }.
}

impl Into<String> for Char {
  fn into(self) -> String = {
    internal_ctos(self)
  }.
}

impl Into<String> for Bool {
  fn into(self) -> String = {
    match self {
      true => "true",
      false => "false",
    }
  }.
}

impl<_T> Into<String> for [_T] where _T: Into<String> {
  fn into(self) -> String = {
    let s = "[".
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
  }.
}

trait Range<_T> {
  type RangeKind.
  fn range(self, t: _T) -> <Self as Range<_T>>::RangeKind.
}

impl Range<()> for Int {
  type RangeKind = RangeIterator.
  fn range(self, t: ()) -> RangeIterator =
    RangeIterator!Infinite(self).
}

impl Range<Int> for Int {
  type RangeKind = RangeIterator.
  fn range(self, t: Int) -> RangeIterator =
    RangeIterator!Finite(self, t).
}

/* TODO: Not really an internal fn, but I can't resize arrays that have slices to them. It's too risky!! */
fn internal_resize_array<_T>(a: [_T], n: Int) -> [_T] = {
  let a_new = internal_alloc_empty_array:<_T>(n).

  for (i, x) in a:iterator():enumerate():limit(n) {
    a_new[i] = x.
  }

  a_new
}.