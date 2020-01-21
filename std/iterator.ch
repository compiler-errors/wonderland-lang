trait Iterable {
  type IterType: Iterator.

  fn iterator(self) -> <Self as Iterable>::IterType.
}

trait Iterator {
  type IterItem.

  fn next(self) -> <Self as Iterator>::IterItem.
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
  fn from_iterator<_It>(it: _It) -> Self where _It: Iterator<::IterItem=_I>.
}