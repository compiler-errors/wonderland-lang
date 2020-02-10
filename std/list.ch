object List<_T> {
  size: Int.
  root: Option<Link<_T>>.
  end: Option<Link<_T>>.
}

object Link<_T> {
  item: _T.
  last: Option<Link<_T>>.
  next: Option<Link<_T>>.
}

impl<_T> for List<_T> {
  fn new() -> List<_T> =
    allocate List { size: 0, root: Option!None, end: Option!None }.

  fn push_front(self, item: _T) {
    self:size = self:size + 1.

    match self:root {
      Option!None => {
        self:root = self:end = Option!Some(Link:new(item)).
      },
      Option!Some(old_root) => {
        let new_root = Link:new(item).
        new_root:link_before(old_root).
        self:root = Option!Some(new_root).
      }
    }
  }

  fn push_back(self, item: _T) {
    self:size = self:size + 1.

    match self:end {
      Option!None => {
        self:root = self:end = Option!Some(Link:new(item)).
      },
      Option!Some(old_end) => {
        let new_end = Link:new(item).
        new_end:link_after(old_end).
        self:end = Option!Some(new_end).
      }
    }
  }

  fn pop_front(self) -> Option<_T> {
    match self:root {
      Option!None => Option!None,
      Option!Some(old_root) => {
        let next_root = old_root:next.
        old_root:unlink().

        if next_root:is_none() {
          self:root = self:end = Option!None.
        }

        self:size = self:size - 1.
        Option!Some(old_root:item)
      }
    }
  }

  fn pop_back(self) -> Option<_T> {
    match self:end {
      Option!None => Option!None,
      Option!Some(old_end) => {
        let next_end = old_end:last.
        old_end:unlink().

        if next_end:is_none() {
          self:root = self:end = Option!None.
        }

        self:size = self:size - 1.
        Option!Some(old_end:item)
      }
    }
  }

  fn get(self, idx: Int) -> Option<_T> {
    match Self:get_node_internal(self:root, idx) {
      Option!Some(node) => Option!Some(node:item),
      Option!None => Option!None,
    }
  }

  fn put(self, idx: Int, item: _T) {
    if idx == 0 {
      self:push_front(item).
    } else if idx == self:size {
      self:push_back(item).
    } else if idx < 0 {
      panic:<()>("Invalid index to insert at: \(idx)").
    } else {
      match Self:get_node_internal(self:root, idx - 1) {
        Option!Some(node) => {
          let new_link = Link:new(item).
          node:link_after(new_link).
        },
        Option!None => {
          panic:<()>("Invalid index to insert at: \(idx)").
        },
      }
    }
  }

  fn remove(self, idx: Int) -> _T {
    if idx < 0 | idx >= self:size {
      panic:<()>("Invalid index to insert at: \(idx)").
    }

    if idx == 0 {
      self:pop_front():unwrap()
    } else if idx == self:size - 1 {
      self:pop_back():unwrap()
    } else {
      match Self:get_node_internal(self:root, idx - 1) {
        Option!Some(node) => {
          node:unlink().
          node:item
        },
        Option!None => {
          panic("Invalid index to insert at: \(idx)")
        },
      }
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

impl<_T> for Link<_T> {
  fn new(item: _T) -> Link<_T> =
    allocate Link { item, last: Option!None, next: Option!None }.

  fn link_after(self, new_next: Link<_T>) {
    let old_next = self:next.
    self:next = Option!Some(new_next).
    new_next:last = Option!Some(self).
    new_next:next = old_next.

    match old_next {
      Option!Some(old_next) => {
        old_next:last = Option!Some(new_next).
      },
      Option!None => {}
    }
  }

  fn link_before(self, new_last: Link<_T>) {
    let old_last = self:last.
    self:last = Option!Some(new_last).
    new_last:next = Option!Some(self).
    new_last:last = old_last.

    match old_last {
      Option!Some(old_last) => {
        old_last:next = Option!Some(new_last).
      },
      Option!None => {}
    }
  }

  fn unlink(self) {
    match self:last {
      Option!Some(last) => {
        last:next = self:next.
      },
      Option!None => {},
    }

    match self:next {
      Option!Some(next) => {
        next:last = self:last.
      },
      Option!None => {},
    }

    self:next = self:last = Option!None.
  }

  fn get(self) -> _T = self:item.

  fn set(self, t: _T) {
    self:item = t.
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
