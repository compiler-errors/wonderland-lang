use pub mod::operators::*.
use pub mod::iterator::*.
use pub mod::option::*.
use pub mod::vector::*.
use pub mod::list::*.
use pub mod::hash_map::*.

export fn gc().

export fn print(s: String).

fn println(s: String) {
  print(s + "\n").
}

fn unreachable<_T>() -> _T {
  panic("Unreachable")
}

fn panic<_T>(s: String) -> _T {
  println("PANIC: " + s).

  exit:<_T>(-1)
}

fn type_string<_T>() -> String {
  instruction "ch_typestring"(:_T) -> String
}

fn exit<_T>(i: Int) -> _T {
    instruction "call" ("exit", i) -> ().
    instruction "ch_undefined" (:_T) -> _T
}
