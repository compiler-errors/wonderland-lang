
object Foo<_T> {}

trait Bar<_T> {
    fn func(self, t: _T).
}

impl<_T> Bar<_T> for Foo<_T> where _T: Bar {}
impl Bar<Int> for Int {}

fn main() {
    <Int>:func(1, 1).
}
