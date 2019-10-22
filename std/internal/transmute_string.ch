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
        "(" + self:0:into() + ",)"
    }
}

impl<_T, _S> Into<String> for (_T, _S) where _T: Into<String>,
                                             _S: Into<String> {
    fn into(self) -> String {
        "(" + self:0:into() + ", " + self:1:into() + ")"
    }
}

impl<_T> Into<String> for [_T] where _T: Into<String> {
    fn into(self) -> String {
        let s = "[".
        let i = 0.

        while i < self:len() {
            if i > 0 {
                s = s + ", ".
            }

            s = s + self[i]:into().
            i = i + 1.
        }

        s + "]"
    }
}