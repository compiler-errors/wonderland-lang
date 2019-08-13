
object<_T> Foo {}

trait<_T> Bar {
    fn func(t: _T).
}

impl<_T> Bar<_T> for Foo<_T> where _T: Bar {}
impl Bar for Int {}

fn main() {
    Foo<_>:func(1).
}
