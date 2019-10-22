use pub mod::operators::*.
use pub mod::transmute::*.

export fn gc().

export fn print(s: String).

fn println(s: String) {
    print(s + "\n").
}