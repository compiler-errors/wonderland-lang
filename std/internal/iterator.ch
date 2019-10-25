impl<_T> Iterable for [_T] {
    type IterType = ArrayIterator<_T>.

    fn iterator(self) -> ArrayIterator<_T> {
        let i = allocate ArrayIterator<_T>.
        i:array = self.
        i:idx = 0.
        i
    }
}

object ArrayIterator<_T> {
    array: [_T].
    idx: Int.
}

impl<_T> Iterator for ArrayIterator<_T> {
    type IterItem = _T.

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
}