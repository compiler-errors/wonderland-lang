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