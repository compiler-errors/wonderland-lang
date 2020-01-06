impl<_It> Iterable for _It where _It: Iterator {
    type IterType = Self.

    fn iterator(self) -> Self = self.
}

impl<_T> Iterable for [_T] {
    type IterType = ArrayIterator<_T>.

    fn iterator(self) -> ArrayIterator<_T> {
        allocate ArrayIterator<_T> { array: self, idx: 0 }
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

    fn size_hint(self) -> Int {
        self:array:len() - self:idx
    }
}


impl Iterable for String {
    type IterType = StringIterator.

    fn iterator(self) -> StringIterator {
        allocate StringIterator { str: self, idx: 0 }
    }
}

object StringIterator {
    str: String.
    idx: Int.
}

impl Iterator for StringIterator {
    type IterItem = Char.

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
    type IterType = RangeIterator.

    fn iterator(self) -> RangeIterator {
        match self {
            Range!Finite(start, end) -> allocate RangeIterator { idx: start, end: Option!Some(end) },
            Range!Infinite(start) -> allocate RangeIterator { idx: start, end: Option!None },
        }
    }
}

object RangeIterator {
    idx: Int.
    end: Option<Int>.
}

impl Iterator for RangeIterator {
    type IterItem = Int.

    fn next(self) -> Int {
        self:idx = self:idx + 1.
        self:idx - 1
    }

    fn has_next(self) -> Bool {
        match self:end {
            Option!Some(end) -> self:idx < end,
            Option!None -> true,
        }
    }

    fn size_hint(self) -> Int {
        match self:end {
            Option!Some(end) -> end - self:idx,
            Option!None -> 0,
        }
    }
}