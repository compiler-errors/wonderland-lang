impl<_It> IterAdapter for _It where _It: Iterator {
    fn map<_F>(self, f: _F) -> Map<_It, _F> = allocate Map { fun: f, iterator: self }.
    fn enumerate(self) -> Enumerate<_It> = allocate Enumerate { idx: 0, iterator: self }.
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