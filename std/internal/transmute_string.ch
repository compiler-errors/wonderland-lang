// Transmute all basic types to string
export fn int_into_string(i: Int) -> String.
export fn char_into_string(c: Char) -> String.

impl Into<String> for Int {
    fn into(self) -> String {
        int_into_string(self)
    }
}

impl Into<String> for Char {
    fn into(self) -> String {
        char_into_string(self)
    }
}

impl From<Bool> for String {
    fn from(b: Bool) -> String {
        if b {
            "true"
        } else {
            "false"
        }
    }
}

impl From<()> for String {
    fn from(u: ()) -> String {
        "()"
    }
}

impl<_T> From<(_T,)> for String {
    fn from(t: (_T,)) -> String {
        "(" + t:0:into() + ",)"
    }
}

impl<_T, _S> From<(_T, _S)> for String {
    fn from(t: (_T, _S)) -> String {
        "(" + t:0:into() + ", " + t:1:into() + ")"
    }
}
