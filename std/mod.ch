use pub mod::operators::*.
use pub mod::transmute::*.
use pub mod::iterator::*.

export fn gc().

export fn print(s: String).

fn println(s: String) {
    print(s + "\n").
}