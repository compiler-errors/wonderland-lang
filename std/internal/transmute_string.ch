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
