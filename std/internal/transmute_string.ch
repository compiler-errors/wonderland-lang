// Transmute all basic types to string
export fn int_into_string_llvm(i: Int) -> String.
export fn float_into_string_llvm(f: Float) -> String.
export fn char_into_string_llvm(c: Char) -> String.

impl Into<String> for Int {
  fn into(self) -> String {
    impl "llvm" {
      int_into_string_llvm(self)
    } else impl "looking_glass" {
      instruction "int_to_string" (self) -> String
    }
  }
}

impl Into<String> for Float {
  fn into(self) -> String {
    impl "llvm" {
      float_into_string_llvm(self)
    } else impl "looking_glass" {
      instruction "float_to_string" (self) -> String
    }
  }
}

impl Into<String> for Char {
  fn into(self) -> String {
    impl "llvm" {
      char_into_string_llvm(self)
    } else impl "looking_glass" {
      instruction "char_to_string" (self) -> String
    }
  }
}

impl Into<String> for Bool {
  fn into(self) -> String {
      match self {
        true => "true",
        false => "false",
      }
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

impl<_T> Into<String> for Option<_T> where _T: Into<String> {
  fn into(self) -> String {
    match self {
      Option!Some(s) => "Some(\(s))",
      Option!None => "None",
    }
  }
}
