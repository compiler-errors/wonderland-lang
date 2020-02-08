object List<_T> {
  size: Int.
  root: Option<Link<_T>>.
  end: Option<Link<_T>>.
}

object Link<_T> {
  item: _T.
  next: Option<Link<_T>>.
}

impl<_T> for List<_T> {
  fn new() -> List<_T> =
    allocate List { size: 0, root: Option!None, end: Option!None }.
}

impl<_T> for Link<_T> {
  fn new(item: _T) -> Link<_T> = allocate Link { item, next: Option!None }.

  fn get(self) -> _T = self:item.
  fn set(self, t: _T) {
    self:item = t.
  }
}

impl<_T> for List<_T> {
  fn push(self, item: _T) {
    self:size = self:size + 1.

    match self:end {
      Option!None => {
        self:root = self:end = Option!Some(Link:new(item)).
      },
      Option!Some(old_end) => {
        self:end = old_end:next = Option!Some(Link:new(item)).
      }
    }
  }

  fn pop(self) -> Option<_T> {
    match self:root {
      Option!None => Option!None,
      Option!Some(old_root) => {
        self:root = old_root:next.
        Option!Some(old_root:item)
      }
    }
  }

  fn get(self, idx: Int) -> Option<_T> {
    match Self:get_node_internal(self:root, idx) {
      Option!Some(node) => Option!Some(node:item),
      Option!None => Option!None,
    }
  }

  fn get_node_internal(node: Option<Link<_T>>, idx: Int) -> Option<Link<_T>> {
    match (node, idx) {
      (node, 0) => node,
      (Option!Some(node), idx) => Self:get_node_internal(node:next, idx - 1),
      (Option!None, _) => Option!None,
    }
  }
}

impl<_T> Len for List<_T> {
    fn len(self) -> Int = self:size.
}

impl<_T> Deref for List<_T> {
  type Idx = Int.
  type Result = _T.

  fn deref(self, idx: Int) -> _T {
      if idx < 0 | idx >= self:len() {
          panic:<()>("Index \(idx) out of bounds. Size is \(self:len())!").
      }

      Self:get_node_internal(self:root, idx):unwrap():item
  }
}

impl<_T> DerefAssign for List<_T> {
  type Idx = Int.
  type Value = _T.

  fn deref_assign(self, idx: Int, value: _T) -> _T {
      if idx >= self:len() {
          panic:<()>("Index \(idx) out of bounds. Size is \(self:len())!").
      }

      Self:get_node_internal(self:root, idx):unwrap():item = value.
      value
  }
}

impl<_T> Iterable for List<_T> {
  type Iterator = ListIterator<_T>.
  type Item = _T.

  fn iterator(self) -> ListIterator<_T> =
    allocate ListIterator { current: self:root, size: self:size }.
}

object ListIterator<_T> {
  current: Option<Link<_T>>.
  size: Int.
}

impl<_T> Iterator for ListIterator<_T> {
  type Item = _T.

  fn next(self) -> _T {
    let current = self:current:unwrap().
    let e = current:item.

    self:current = current:next.
    self:size = self:size - 1.
    e
  }

  fn has_next(self) -> Bool = self:current:is_some().
  fn size_hint(self) -> Int = self:size.
}

impl<_T> Into<String> for List<_T> where _T: Into<String> {
  fn into(self) -> String {
      let s = "List[".
      let first = true.

      for i in self {
          if first {
              first = false.
          } else {
              s = s + ", ".
          }

          s = s + (i as String).
      }

      s + "]"
  }
}
