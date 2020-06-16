enum Option<_T> {
  Some(_T),
  None,
}

impl<_T> for Option<_T> {
  fn map<_F, _O>(self, f: _F) -> Option<_O> where _F: Fn(_T) -> _O = {
    match self {
      Option!Some(t) => Option!Some(f(t)),
      Option!None => Option!None,
    }
  }.

  fn expect(self, s: String) -> _T = {
    match self {
      Option!Some(v) => v,
      _ => panic(s),
    }
  }.

  fn unwrap(self) -> _T = {
    self:expect("No value for \(type_string:<Self>())")
  }.

  fn unwrap_or(self, other: _T) -> _T = {
    match self {
      Option!Some(v) => v,
      _ => other,
    }
  }.

  fn unwrap_or_else<_F>(self, otherwise: _F) -> _T where _F: Fn() -> _T = {
    match self {
      Option!Some(v) => v,
      _ => otherwise(),
    }
  }.

  fn is_some(self) -> Bool = {
    match self {
      Option!Some(_) => true,
      Option!None => false,
    }
  }.

  fn is_none(self) -> Bool = {
      !self:is_some()
  }.
}

impl<_T> Into<String> for Option<_T> where _T: Into<String> {
  fn into(self) -> String = {
    match self {
      Option!Some(s) => "Some(\(s))",
      Option!None => "None",
    }
  }.
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

enum Either<_L, _R> {
  Left(_L),
  Right(_R),
}

impl<_L, _R> for Either<_L, _R> {
  fn expect_left(self, s: String) -> _L =
    match self {
      Either!Left(v) => v,
      _ => panic(s),
    }.

  fn unwrap_left(self) -> _L =
    self:expect_left("No Left value for \(type_string:<Self>())").

  fn map_left<_F, _O>(self, f: _F) -> Either<_O, _R> where _F: Fn(_L) -> _O =
    match self {
      Either!Left(l) => Either!Left(f(l)),
      Either!Right(r) => Either!Right(r),
    }.

  fn is_left(self) -> Bool =
    match self {
      Either!Left(_) => true,
      Either!Right(_) => false,
    }.

  fn expect_right(self, s: String) -> _R =
    match self {
      Either!Right(v) => v,
      _ => panic(s),
    }.

  fn unwrap_right(self) -> _R =
    self:expect_right("No Right value for \(type_string:<Self>())").

  fn map_right<_F, _O>(self, f: _F) -> Either<_L, _O> where _F: Fn(_R) -> _O =
    match self {
      Either!Left(l) => Either!Left(l),
      Either!Right(r) => Either!Right(f(r)),
    }.

  fn is_right(self) -> Bool =
    match self {
      Either!Left(_) => false,
      Either!Right(_) => true,
    }.

  fn map<_F, _G, _OL, _OR>(self, f: _F, g: _G) -> Either<_OL, _OR>
    where _F: Fn(_L) -> _OL, _G: Fn(_R) -> _OR = match self {
      Either!Left(l) => Either!Left(f(l)),
      Either!Right(r) => Either!Right(g(r)),
    }.

  fn unify<_F, _G, _O>(self, f: _F, g: _G) -> _O
    where _F: Fn(_L) -> _O, _G: Fn(_R) -> _O = match self {
      Either!Left(l) => f(l),
      Either!Right(r) => g(r),
    }.
}