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

trait IterAdapter: Iterator {
    fn map<_F>(self, f: _F) -> Map<Self, _F>.
    fn enumerate(self) -> Enumerate<Self>.
    fn limit(self, n: Int) -> Limit<Self>.
    fn collect<_C>(self) -> _C where _C: FromIterator<<Self as Iterator>::IterItem>.
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
    idx: Int.
    limit: Int.
    iterator: _It.
}

trait FromIterator<_I> {
    fn from_iterator<_It>(it: _It) -> Self where _It: Iterator<::IterItem=_I>.
}