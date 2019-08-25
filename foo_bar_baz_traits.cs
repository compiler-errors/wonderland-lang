trait Foo {
    type Item.
    fn k() -> Self::Item.
}
trait Bar: Foo {}
trait Baz: Foo {}
trait Bing: Bar + Baz + Foo {}

fn main() {}
