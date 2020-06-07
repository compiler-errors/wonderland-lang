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

  fn next(self) -> (Option<<Self as Iterator>::Item>, Self).
  fn has_next(self) -> Bool.
  fn size_hint(self) -> Int.
}

trait FromIterator<_I> {
  fn from_iterator<_It>(it: _It) -> Self where _It: Iterator<::Item=_I>.
}

enum RangeIterator {
  Finite(Int, Int),
  Infinite(Int),
}

impl Iterator for RangeIterator {
  type Item = Int.

  fn next(self) -> (Option<Int>, RangeIterator) = {
      match self {
        RangeIterator!Finite(a, b) =>
          if a < b {
            (Option!Some(a), RangeIterator!Finite(a + 1, b))
          } else {
            (Option!None, self)
          },
        RangeIterator!Infinite(a) => (Option!Some(a), RangeIterator!Infinite(a + 1)),
      }
  }.

  fn has_next(self) -> Bool = {
    match self {
      RangeIterator!Finite(a, b) => a < b,
      RangeIterator!Infinite(_) => true,
    }
  }.

  fn size_hint(self) -> Int = {
    match self {
      RangeIterator!Finite(a, b) => b - a,
      RangeIterator!Infinite(_) => -1,
    }
  }.
}

impl<_It> for _It where _It: Iterator {
  fn map<_F>(self, fun: _F) -> Map<_It, _F> = Map!Iterator { fun, iterator: self }.
  fn enumerate(self) -> Enumerate<_It> = Enumerate!Iterator { idx: 0, iterator: self }.
  fn limit(self, limit: Int) -> Limit<_It> = Limit!Iterator { remaining: limit, iterator: self }.
  fn zip<_It2>(self, other: _It2) -> Zip<_It, _It2> = Zip!Iterator(self, other).

  fn collect<_C>(self) -> _C where _C: FromIterator<<Self as Iterator>::Item> = {
      <_C>:from_iterator(self)
  }.

  fn fold<_F, _I>(self, i: _I, f: _F) -> _I where _F: Fn(_I, <Self as Iterator>::Item) -> _I = {
      for j in self {
          i = f(i, j).
      }

      i
  }.
}

impl<_T> for _T where _T: Iterator, <Self as Iterator>::Item: Default + Add<<Self as Iterator>::Item, ::Result=<Self as Iterator>::Item> {
  fn sum(self) -> <Self as Iterator>::Item =
    Self:fold(
      self,
      <Self as Iterator>::Item:default(),
      |a, b| a + b).
}

enum Map<_It, _F> {
  Iterator {
    iterator: _It,
    fun: _F,
  },
}

impl<_It, _F, _O> Iterator for Map<_It, _F> where
  _It: Iterator,
  _F: Fn(<_It as Iterator>::Item) -> _O {
  type Item = _O.

  fn next(self) -> (Option<_O>, Map<_It, _F>) = {
      let Map!Iterator { iterator, fun } = self.

      let (next, iterator) = iterator:next().

      (next:map(fun), Map!Iterator { iterator, fun })
  }.

  fn has_next(self) -> Bool = {
      let Map!Iterator { iterator, ... } = self.
      iterator:has_next()
  }.

  fn size_hint(self) -> Int = {
      let Map!Iterator { iterator, ... } = self.
      iterator:size_hint()
  }.
}

enum Enumerate<_It> {
  Iterator {
    iterator: _It,
    idx: Int,
  },
}

impl<_It> Iterator for Enumerate<_It> where _It: Iterator {
  type Item = (Int, <_It as Iterator>::Item).

  fn next(self) -> (Option<(Int, <_It as Iterator>::Item)>, Enumerate<_It>) = {
    let Enumerate!Iterator { iterator, idx } = self.

    let (next, iterator) = iterator:next().

    match next {
      Option!Some(next) => (Option!Some((idx, next)), Enumerate!Iterator { iterator, idx: idx + 1 }),
      Option!None => (Option!None, Enumerate!Iterator { iterator, idx })
    }
  }.

  fn has_next(self) -> Bool = {
      let Enumerate!Iterator { iterator, ... } = self.
      iterator:has_next()
  }.

  fn size_hint(self) -> Int = {
      let Enumerate!Iterator { iterator, ... } = self.
      iterator:size_hint()
  }.
}

enum Limit<_It> {
  Iterator {
    iterator: _It,
    remaining: Int,
  },
}

impl<_It> Iterator for Limit<_It> where _It: Iterator {
  type Item = <_It as Iterator>::Item.

  fn next(self) -> (Option<<Self as Iterator>::Item>, Limit<_It>) = {
      let Limit!Iterator { iterator, remaining } = self.

      if remaining <= 0 {
        (Option!None, Limit!Iterator { iterator, remaining })
      } else {
        let (next, iterator) = iterator:next().

        (next, Limit!Iterator { iterator, remaining: remaining - 1 })
      }
  }.

  fn has_next(self) -> Bool = {
    let Limit!Iterator { iterator, remaining } = self.

    if remaining <= 0 {
      false
    } else {
      iterator:has_next()
    }
  }.

  fn size_hint(self) -> Int = {
    let Limit!Iterator { iterator, remaining } = self.
    let hint = iterator:size_hint().

    if hint < 0 {
      remaining
    } else {
      min(remaining, hint)
    }
  }.
}

enum Zip<_I1, _I2> {
  Iterator(_I1, _I2),
}

impl<_I1, _I2> Iterator for Zip<_I1, _I2> where _I1: Iterator, _I2:Iterator {
  type Item = (<_I1 as Iterator>::Item, <_I2 as Iterator>::Item).

  fn next(self) -> (Option<<Self as Iterator>::Item>, Self) = {
    let Zip!Iterator(i1, i2) = self.

    if i1:has_next() &? i2:has_next() {
      let (n1, i1) = i1:next().
      let (n2, i2) = i2:next().

      (Option!Some((n1:unwrap(), n2:unwrap())), Zip!Iterator(i1, i2))
    } else {
      (Option!None, Zip!Iterator(i1, i2))
    }
  }.

  fn has_next(self) -> Bool = {
    let Zip!Iterator(i1, i2) = self.
    i1:has_next() &? i2:has_next()
  }.

  fn size_hint(self) -> Int = {
    let Zip!Iterator(i1, i2) = self.
    match (i1:size_hint(), i2:size_hint()) {
      (-1, -1) => -1,
      (-1, b)  => b,
      (a, -1)  => a,
      (a, b)   => min(a, b),
    }
  }.
}

impl<_T> Iterable for [_T] {
  type Iterator = ArrayIterator<_T>.
  type Item = _T.

  fn iterator(self) -> ArrayIterator<_T> = {
      ArrayIterator!Iterator { array: self, idx: 0 }
  }.
}

enum ArrayIterator<_T> {
  Iterator {
    array: [_T],
    idx: Int,
  },
}

impl<_T> Iterator for ArrayIterator<_T> {
  type Item = _T.

  fn next(self) -> (Option<_T>, ArrayIterator<_T>) = {
      let ArrayIterator!Iterator { array, idx } = self.
      let len = array:len().

      if idx >= len {
        (Option!None, ArrayIterator!Iterator { array, idx })
      } else {
        (Option!Some(array[idx]), ArrayIterator!Iterator { array, idx: idx + 1 })
      }
  }.

  fn has_next(self) -> Bool = {
    let ArrayIterator!Iterator { array, idx } = self.

    idx < array:len()
  }.

  fn size_hint(self) -> Int = {
    let ArrayIterator!Iterator { array, idx } = self.

    array:len() - idx
  }.
}

impl<_I> FromIterator<_I> for [_I] {
  fn from_iterator<_It>(it: _It) -> [_I] where _It: Iterator<::Item=_I> = {
      let s = it:size_hint().
      let last = -1.

      let a = allocate_empty_array_internal:<_I>(s).

      for (i, x) in it:enumerate() {
          if i >= a:len() {
              a = resize_array_internal(a, a:len() * 2).
          }

          a[i] = x.
          last = i.
      }

      if a:len() == last + 1 {
          a
      } else {
          resize_array_internal(a, last + 1)
      }
  }.
}

fn resize_array_internal<_T>(a: [_T], n: Int) -> [_T] = {
  let a_new = allocate_empty_array_internal:<_T>(n).

  for (i, x) in a:iterator():enumerate():limit(n) {
      a_new[i] = x.
  }

  a_new
}.

enum StringIterator {
  Iterator {
    str: String,
    idx: Int,
  },
}

impl Iterable for String {
  type Iterator = StringIterator.
  type Item = Char.

  fn iterator(self) -> StringIterator = {
      StringIterator!Iterator { str: self, idx: 0 }
  }.
}

impl Iterator for StringIterator {
  type Item = Char.

  fn next(self) -> (Option<Char>, StringIterator) = {
      let StringIterator!Iterator { str, idx } = self.

      if idx >= str:len() {
        (Option!None, StringIterator!Iterator { str, idx })
      } else {
        (Option!Some(str[idx]), StringIterator!Iterator { str, idx: idx + 1 })
      }
  }.

  fn has_next(self) -> Bool = {
    let StringIterator!Iterator { str, idx } = self.

    idx < str:len()
  }.

  fn size_hint(self) -> Int = {
    let StringIterator!Iterator { str, idx } = self.

    str:len() - idx
  }.
}

impl FromIterator<Char> for String {
  fn from_iterator<_It>(it: _It) -> String where _It: Iterator<::Item=Char> = {
      let s = "a".

      for c in it {
          s = s + (c as String).
      }

      s
  }.
}
