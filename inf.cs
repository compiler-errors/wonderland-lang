trait Foo {
    type I: Bar.
}

trait Bar {
    type J: Foo.
}

// This will cause an infinite loop. :(
fn blam<_G>() where _G: Foo {}
