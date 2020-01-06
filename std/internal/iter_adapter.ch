use std::internal::cursed::cursed_allocate_array.

impl<_It> IterAdapter for _It where _It: Iterator {
    fn map<_F>(self, f: _F) -> Map<_It, _F> = allocate Map { fun: f, iterator: self }.
    fn enumerate(self) -> Enumerate<_It> = allocate Enumerate { idx: 0, iterator: self }.
    fn limit(self, limit: Int) -> Limit<_It> = allocate Limit { idx: 0, limit, iterator: self }.

    fn collect<_C>(self) -> _C where _C: FromIterator<<Self as Iterator>::IterItem> {
        <_C>:from_iterator(self)
    }
}

impl<_It, _F, _O> Iterator for Map<_It, _F> where
  _It: Iterator,
  _F: Fn(<_It as Iterator>::IterItem) -> _O {
    type IterItem = _O.

    fn next(self) -> _O = (self:fun)(self:iterator:next()).
    fn has_next(self) -> Bool = self:iterator:has_next().
    fn size_hint(self) -> Int = self:iterator:size_hint().
}

impl<_It> Iterator for Enumerate<_It> where _It: Iterator {
    type IterItem = (Int, <_It as Iterator>::IterItem).

    fn next(self) -> <Self as Iterator>::IterItem {
        let idx = self:idx.
        self:idx = self:idx + 1.

        (idx, self:iterator:next())
    }

    fn has_next(self) -> Bool = self:iterator:has_next().
    fn size_hint(self) -> Int = self:iterator:size_hint().
}

impl<_It> Iterator for Limit<_It> where _It: Iterator {
    type IterItem = <_It as Iterator>::IterItem.

    fn next(self) -> <Self as Iterator>::IterItem {
        self:idx = self:idx + 1.
        self:iterator:next()
    }

    fn has_next(self) -> Bool = self:iterator:has_next() & self:idx < self:limit.

    fn size_hint(self) -> Int {
        let limited = self:idx - self:limit.
        let underlying = self:iterator:size_hint().

        min(limited, underlying)
    }
}

impl<_I> FromIterator<_I> for [_I] {
    fn from_iterator<_It>(it: _It) -> [_I] where _It: Iterator<::IterItem=_I> {
        let s = it:size_hint().
        if s < 1 {
            s = 1.
        }

        let last = -1.

        // println("Initial alloc is " + s:into()).
        let a = cursed_allocate_array:<_I>(s).

        for (i, x) in it:enumerate() {
            if i >= a:len() {
                // println("Re-alloc is " + (a:len() * 2):into()).
                a = resize_array(a, a:len() * 2).
            }

            a[i] = x.
            last = i.
        }

        if a:len() == last + 1 {
            a
        } else {
            // println("Final alloc is " + (last + 1):into()).
            resize_array(a, last + 1)
        }
    }
}

fn resize_array<_T>(a: [_T], n: Int) -> [_T] {
    let a_new = cursed_allocate_array:<_T>(n).

    for (i, x) in a:iterator():enumerate():limit(n) {
        a_new[i] = x.
    }

    a_new
}
