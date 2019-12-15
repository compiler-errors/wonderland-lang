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
}

object Map<_It, _F> {
    iterator: _It.
    fun: _F.
}

object Enumerate<_It> {
    idx: Int.
    iterator: _It.
}