enum Option<_T> {
  Some(_T),
  None,
}

impl<_T> for Option<_T> {
  fn map<_F, _O>(self, f: _F) -> Option<_O> where _F: Fn(_T) -> _O {
    match self {
      Option!Some(t) => Option!Some(f(t)),
      Option!None => Option!None,
    }
  }

  fn unwrap(self) -> _T {
      match self {
          Option!Some(v) => v,
          _ => panic("No value for \(type_string:<Self>())"),
      }
  }

  fn unwrap_or(self, other: _T) -> _T {
      match self {
          Option!Some(v) => v,
          _ => other,
      }
  }

  fn unwrap_or_else<_F>(self, otherwise: _F) -> _T where _F: Fn() -> _T {
      match self {
          Option!Some(v) => v,
          _ => otherwise(),
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

impl Default for Int {
  fn default() -> Int = 0.
}

impl Default for Char {
  fn default() -> Char = ' '.
}

impl Default for Bool {
  fn default() -> Bool = false.
}

impl Default for String {
  fn default() -> String = "".
}
