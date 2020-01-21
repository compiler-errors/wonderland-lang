use pub mod::operators::*.
use pub mod::iterator::*.
use pub mod::option::*.

export fn gc().

export fn print(s: String).

fn println(s: String) {
  print(s + "\n").
}

fn panic<_T>(s: String) -> _T {
  println(s).

  match s {}
}