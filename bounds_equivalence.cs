trait Foo {
    fn foo<_G>() -> _G where _G: Foo.
}

trait Bar {
    type Item.
    type Other.

    fn baz() where Self::Item: Foo, Self::Other: Foo.
}

impl Bar for () {
    type Item = Int.
    type Other = Char.

    fn baz() where Int: Foo, Self::Other: Foo {}
}

impl Bar for Int {
    type Item = ().
    type Other = ().

    fn baz() where (): Foo {}
}

fn main() {
    let i: Int = <_>:foo().
}
