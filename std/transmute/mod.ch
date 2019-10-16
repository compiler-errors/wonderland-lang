trait Into<_T> {
    fn into(self) -> _T.
}

trait From<_U> {
    fn from(u: _U) -> Self.
}

impl<_T, _U> Into<_T> for _U where _T: From<_U> {
    fn into(self) -> _T {
        _T:from(self)
    }
}

impl<_U> From<_U> for _U {
    fn from(u: _U) -> _U {
        u
    }
}