enum Result<_T, _E> {
    Ok(_T),
    Error(_E),
}

impl<_T, _E> Into<String> for Result<_T, _E> where _T: Into<String>, _E: Into<String> {
  fn into(self) -> String = {
    match self {
      Result!Ok(s) => "Ok(\(s))",
      Result!Error(s) => "Error(\(s))",
    }
  }.
}

trait IntoResult {
    type OkType.
    type ErrorType.

    fn into_result(self) -> Result<<Self as IntoResult>::OkType, <Self as IntoResult>::ErrorType>.

    fn from_ok(t: <Self as IntoResult>::OkType) -> Self.
    fn from_error(e: <Self as IntoResult>::ErrorType) -> Self.
}

impl<_T, _E> IntoResult for Result<_T, _E> {
    type OkType = _T.
    type ErrorType = _E.

    fn into_result(self) -> Result<_T, _E> = self.
    fn from_ok(t: _T) -> Result<_T, _E> = Result!Ok(t).
    fn from_error(e: _E) -> Result<_T, _E> = Result!Error(e).
}

impl<_T> IntoResult for Option<_T> {
    type OkType = _T.
    type ErrorType = ().

    fn into_result(self) -> Result<_T, ()> = match self {
        Option!Some(t) => Result!Ok(t),
        Option!None => Result!Error(()),
    }.

    fn from_ok(t: _T) -> Option<_T> = Option!Some(t).
    fn from_error(e: ()) -> Option<_T> = Option!None.
}

