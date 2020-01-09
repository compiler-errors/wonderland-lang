enum Option<_T> {
    Some(_T).
    None.
}

impl<_T> for Option<_T> {
    fn unwrap_or_else<_F>(self, otherwise: _F) -> _T where _F: Fn() -> _T {
        match self {
            Option!Some(v) -> v,
            _ -> otherwise(),
        }
    }
}

trait Default {
    fn default() -> Self.
}