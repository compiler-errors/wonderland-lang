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

impl Into<String> for Bool {
    fn into(self) -> String {
        if self {
            "true"
        } else {
            "false"
        }
    }
}

impl Into<String> for () {
    fn into(self) -> String {
        "()"
    }
}

impl<_T> Into<String> for (_T,) where _T: Into<String> {
    fn into(self) -> String {
        "(" + to_string(self:0) + ",)"
    }
}

impl<_T, _S> Into<String> for (_T, _S) where _T: Into<String>,
                                             _S: Into<String> {
    fn into(self) -> String {
        "(" + to_string(self:0) + ", " + to_string(self:1) + ")"
    }
}

impl<_T> Into<String> for [_T] where _T: Into<String> {
    fn into(self) -> String {
        let s = "[".
        let first = true.

        for i in self {
            if first {
                first = false.
            } else {
                s = s + ", ".
            }

            s = s + to_string(i).
        }

        s + "]"
    }
}

fn to_string<_T>(t: _T) -> String where _T: Into<String> {
    <_T as Into<String>>:into(t)
}