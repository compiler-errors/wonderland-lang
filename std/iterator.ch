trait Iterable {
  type Iterator: Iterator<::Item=<Self as Iterable>::Item>.
  type Item.

  fn iterator(self) -> <Self as Iterable>::Iterator.
}

impl<_It, _T> Iterable for _It where _It: Iterator<::Item=_T> {
  type Iterator = _It.
  type Item = _T.

  fn iterator(self) -> Self = self.
}

trait Iterator {
  type Item.

  fn next(self) -> <Self as Iterator>::Item.
  fn has_next(self) -> Bool.
  fn size_hint(self) -> Int.
}

enum Range {
  Finite(Int, Int).
  Infinite(Int).
}

object Map<_It, _F> {
  iterator: _It.
  fun: _F.
}

object Enumerate<_It> {
  idx: Int.
  iterator: _It.
}

object Limit<_It> {
  limit: Int.
  idx: Int.
  iterator: _It.
}

trait FromIterator<_I> {
  fn from_iterator<_It>(it: _It) -> Self where _It: Iterator<::Item=_I>.
}

impl<_T> Iterable for [_T] {
  type Iterator = ArrayIterator<_T>.
  type Item = _T.

  fn iterator(self) -> ArrayIterator<_T> {
      allocate ArrayIterator<_T> { array: self, idx: 0 }
  }
}

object ArrayIterator<_T> {
  array: [_T].
  idx: Int.
}

impl<_T> Iterator for ArrayIterator<_T> {
  type Item = _T.

  fn next(self) -> _T {
      let e = self:array[self:idx].

      if self:idx < self:array:len() {
          self:idx = self:idx + 1.
      }

      e
  }

  fn has_next(self) -> Bool {
      self:idx < self:array:len()
  }

  fn size_hint(self) -> Int {
      self:array:len() - self:idx
  }
}


impl Iterable for String {
  type Iterator = StringIterator.
  type Item = Char.

  fn iterator(self) -> StringIterator {
      allocate StringIterator { str: self, idx: 0 }
  }
}

object StringIterator {
  str: String.
  idx: Int.
}

impl Iterator for StringIterator {
  type Item = Char.

  fn next(self) -> Char {
      let e = self:str[self:idx].

      if self:idx < self:str:len() {
          self:idx = self:idx + 1.
      }

      e
  }

  fn has_next(self) -> Bool {
      self:idx < self:str:len()
  }

  fn size_hint(self) -> Int {
      self:str:len() - self:idx
  }
}

impl Iterable for Range {
  type Iterator = RangeIterator.
  type Item = Int.

  fn iterator(self) -> RangeIterator {
      match self {
          Range!Finite(start, end) => allocate RangeIterator { idx: start, end: Option!Some(end) },
          Range!Infinite(start) => allocate RangeIterator { idx: start, end: Option!None },
      }
  }
}

object RangeIterator {
  idx: Int.
  end: Option<Int>.
}

impl Iterator for RangeIterator {
  type Item = Int.

  fn next(self) -> Int {
      self:idx = self:idx + 1.
      self:idx - 1
  }

  fn has_next(self) -> Bool {
      match self:end {
          Option!Some(end) => self:idx < end,
          Option!None => true,
      }
  }

  fn size_hint(self) -> Int {
      match self:end {
          Option!Some(end) => end - self:idx,
          Option!None => -1,
      }
  }
}

impl<_It> for _It where _It: Iterator {
  fn map<_F>(self, f: _F) -> Map<_It, _F> = allocate Map { fun: f, iterator: self }.
  fn enumerate(self) -> Enumerate<_It> = allocate Enumerate { idx: 0, iterator: self }.
  fn limit(self, limit: Int) -> Limit<_It> = allocate Limit { idx: 0, limit, iterator: self }.

  fn collect<_C>(self) -> _C where _C: FromIterator<<Self as Iterator>::Item> {
      <_C>:from_iterator(self)
  }

  fn fold<_F, _I>(self, i: _I, f: _F) -> _I where _F: Fn(_I, <Self as Iterator>::Item) -> _I {
      for j in self {
          i = f(i, j).
      }

      i
  }
}

impl<_T> for _T where _T: Iterator, <Self as Iterator>::Item: Default + Add<<Self as Iterator>::Item, ::Result=<Self as Iterator>::Item> {
  fn sum(self) -> <Self as Iterator>::Item =
    Self:fold(
      self,
      <Self as Iterator>::Item:default(),
      |a, b| a + b).
}

impl<_It, _F, _O> Iterator for Map<_It, _F> where
  _It: Iterator,
  _F: Fn(<_It as Iterator>::Item) -> _O {
  type Item = _O.

  fn next(self) -> _O = (self:fun)(self:iterator:next()).
  fn has_next(self) -> Bool = self:iterator:has_next().
  fn size_hint(self) -> Int = self:iterator:size_hint().
}

impl<_It> Iterator for Enumerate<_It> where _It: Iterator {
  type Item = (Int, <_It as Iterator>::Item).

  fn next(self) -> <Self as Iterator>::Item {
      let idx = self:idx.
      self:idx = self:idx + 1.

      (idx, self:iterator:next())
  }

  fn has_next(self) -> Bool = self:iterator:has_next().
  fn size_hint(self) -> Int = self:iterator:size_hint().
}

impl<_It> Iterator for Limit<_It> where _It: Iterator {
  type Item = <_It as Iterator>::Item.

  fn next(self) -> <Self as Iterator>::Item {
      self:idx = self:idx + 1.
      self:iterator:next()
  }

  fn has_next(self) -> Bool = self:iterator:has_next() & self:idx < self:limit.

  fn size_hint(self) -> Int {
      let limited = self:limit - self:idx.
      let underlying = self:iterator:size_hint().

      if underlying < 0 {
          limited
      } else {
          min(limited, underlying)
      }
  }
}

impl<_I> FromIterator<_I> for [_I] {
  fn from_iterator<_It>(it: _It) -> [_I] where _It: Iterator<::Item=_I> {
      let s = it:size_hint().
      let last = -1.

      let a = allocate_empty_array_internal:<_I>(s).

      for (i, x) in it:enumerate() {
          if i >= a:len() {
              // println("Re-alloc is " + (a:len() * 2):into()).
              a = resize_array_internal(a, a:len() * 2).
          }

          a[i] = x.
          last = i.
      }

      if a:len() == last + 1 {
          a
      } else {
          // println("Final alloc is " + (last + 1):into()).
          resize_array_internal(a, last + 1)
      }
  }
}

fn resize_array_internal<_T>(a: [_T], n: Int) -> [_T] {
  let a_new = allocate_empty_array_internal:<_T>(n).

  for (i, x) in a:iterator():enumerate():limit(n) {
      a_new[i] = x.
  }

  a_new
}

impl FromIterator<Char> for String {
  fn from_iterator<_It>(it: _It) -> String where _It: Iterator<::Item=Char> {
      let s = "a".

      for c in it {
          s = s + (c as String).
      }

      s
  }
}
