use pub mod::any::*.
use pub mod::operators::*.
use pub mod::iterator::*.
use pub mod::option::*.
use pub mod::try::*.
use pub mod::vector::*.
use pub mod::list::*.
use pub mod::hash_map::*.

export fn gc_llvm().

export fn print_llvm(s: String).

fn gc() {
  impl "llvm" {
    gc_llvm().
  } else impl "looking_glass" {
    instruction "gc" () -> ().
  }
}

fn print(s: String) {
  impl "llvm" {
    print_llvm(s).
  } else impl "looking_glass" {
    instruction "print" (s) -> ().
  }
}

fn println(s: String) {
  print(s + "\n").
}

fn unreachable<_T>() -> _T {
  panic("Unreachable")
}

fn panic<_T>(s: String) -> _T {
  println("PANIC: " + s).
  instruction "breakpoint" () -> ().

  exit:<_T>(-1)
}

fn type_string<_T>() -> String {
  instruction "ch_typestring"(_ :_T) -> String
}

fn type_string_of<_T>(t: _T) -> String {
  instruction "ch_typestring"(_ :_T) -> String
}

fn exit<_T>(i: Int) -> _T {
    impl "llvm" {
      instruction "call" ("exit", i) -> ().
      instruction "ch_undefined" (_ :_T) -> _T
    } else impl "looking_glass" {
      instruction "exit" (i) -> _T
    }
}

fn breakpoint() {
  instruction "breakpoint" () -> ().
}
