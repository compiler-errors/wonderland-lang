enum Option<_T> {
  Some(_T).
  None.
}

impl<_T> for Option<_T> {
  fn unwrap_or_else<_F>(self, otherwise: _F) -> _T where _F: Fn() -> _T {
      match self {
          Option!Some(v) => v,
          _ => otherwise(),
      }
  }

  fn unwrap(self) -> _T {
      match self {
          Option!Some(v) => v,
          _ => panic("No value for Option"),
      }
  }

  fn is_some(self) -> Bool {
      match self {
          Option!Some(_) => true,
          Option!None => false,
      }
  }

  fn is_none(self) -> Bool {
      !self:is_some()
  }
}

trait Default {
  fn default() -> Self.
}