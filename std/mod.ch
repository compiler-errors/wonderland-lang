use pub mod::any::*.
use pub mod::operators::*.
use pub mod::iterator::*.
use pub mod::option::*.
use pub mod::try::*.
use pub mod::vector::*.
use pub mod::list::*.
use pub mod::hash_map::*.
use pub mod::threading::*.
use pub mod::asynchronous::*.

extern fn gc().
extern fn print(s: String).
extern fn unreachable<_T>() -> _T.
extern fn type_string<_T>() -> String.
extern fn exit(i: Int).
extern fn breakpoint().


fn println(s: String) = {
  print(s + "\n").
}.

fn panic<_T>(s: String) -> _T = {
  println("PANIC: " + s).
  breakpoint().

  exit(-1).
  unreachable()
}.

fn type_string_of<_T>(t: _T) -> String = {
  type_string:<_T>()
}.