export fn internal_format(f: String, a: [String]) -> String.

/* One would use this library like:

```
  println("% + % = %.":format((1, 2, 1 + 2))).
```

  Note that the argument to `format` is a tuple.
  This is key in faking the variadic relationship.

  This is also why we have to impl this trait like
  a billion times.

  One could theoretically implement some more convenience methods, such as "printlnf"

```
  fn printlnf<_T>(f: String, a: _T) where String: Format<_T> {
    println(f:format(a)).
  }

  printlnf("Hello, %! My name is %.", ("michael", "alex")).
```
*/

trait Format<_T> {
    fn format(self, a: _T) -> String.
}

// Convenience method when you want to flip the arguments.
fn format_str<_T>(f: String, a: _T) -> String where String: Format<_T> {
  f:format(a)
}

impl<_A> Format<(_A,)> for String where _A: Into<String> {
    fn format(self, a: (_A,)) -> String {
        internal_format(self, [a:0:into()])
    }
}

impl<_A, _B> Format<(_A, _B)> for String where _A: Into<String>, _B: Into<String> {
    fn format(self, a: (_A, _B)) -> String {
        internal_format(self, [a:0:into(), a:1:into()])
    }
}

// etc.