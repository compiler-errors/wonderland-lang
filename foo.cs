trait Collection {
  type Item.

  fn add(self, t: Self::Item).
  fn remove(self) -> Self::Item.
}

object<_T> Vec { }
object IntVec { }

impl<_T> Collection for Vec<_T> {
  type Item = _T.
}

impl Collection for IntVec {
  type Item = Int.
}

export fn<_T> get_a_collection() -> _T where _T: Collection.

export fn bar(s: IntVec).

fn main() {
  let k = get_a_collection().
  let s = k:remove().

  Collection:add(k, 1).
  bar(k).
}
