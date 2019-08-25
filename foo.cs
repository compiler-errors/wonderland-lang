trait Store {}

trait Collection {
  type Item: Store.

  fn add(self, t: Self::Item).
  fn remove(self) -> Self::Item.
}

object Vec<_T> { }
object IntVec { }

impl Store for Int {}

impl<_T> Collection for Vec<_T> where _T: Store {
  type Item = _T.
      fn add(self, t: _T) {}
  fn remove(self) -> _T {}
}

impl Collection for IntVec {
  type Item = Int.
      fn add(self, t: Int) {}
  fn remove(self) -> Int {}
}

//ffi_call<"add">(a: Int, b: Int, c: Int);
//use core;DD

export fn get_a_collection<_T>() -> _T where _T: Collection.

export fn bar(s: IntVec).

//fn main() {}

fn main() {
  let k: _ = get_a_collection().
  let s = k:remove().

  k:add(1).
  bar(k).
}
