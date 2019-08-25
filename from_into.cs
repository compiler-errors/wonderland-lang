trait From<_T> {
    fn from(t: _T) -> Self.
}

trait Into<_T> {
    fn into(self) -> _T.
}

impl<_T, _S> Into<_S> for _T where _S: From<_T> {
    fn into(self) -> _S {
        return <_>:from(self).
    }
}

impl<_T> From<_T> for _T {
    fn from(t: _T) -> _T {
        return t.
    }
}

impl From<Int> for String {
    fn from(t: Int) -> String {
        return "".
    }
}

fn main() {
    let i = 1.
    let j: String = i:into().
}
