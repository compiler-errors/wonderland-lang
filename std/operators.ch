trait Add<_T> {
    type Result.
    fn add(self, other: _T) -> <Self as Add<_T>>::Result.
}

trait Subtract<_T> {
    type Result.
    fn sub(self, other: _T) -> <Self as Subtract<_T>>::Result.
}

trait Multiply<_T> {
    type Result.
    fn mul(self, other: _T) -> <Self as Multiply<_T>>::Result.
}

trait Divide<_T> {
    type Result.
    fn div(self, other: _T) -> <Self as Divide<_T>>::Result.
}

trait Modulo<_T> {
    type Result.
    fn rem(self, other: _T) -> <Self as Modulo<_T>>::Result.
}

trait PartialCompare<_T> {
    fn compare(self, other: _T) -> Int.
}

trait Compare<_T> {
    fn gt(self, other: _T) -> Bool.
    fn lt(self, other: _T) -> Bool.
    fn ge(self, other: _T) -> Bool.
    fn le(self, other: _T) -> Bool.
}

fn min<_T>(a: _T, b: _T) -> _T where _T: Compare<_T> {
    if a < b {
        a
    } else {
        b
    }
}

fn max<_T>(a: _T, b: _T) -> _T where _T: Compare<_T> {
    if a > b {
        a
    } else {
        b
    }
}

trait Equals<_T> {
    fn eq(self, other: _T) -> Bool.
    fn ne(self, other: _T) -> Bool.
}

trait And<_T> {
    type Result.
    fn and(self, other: _T) -> <Self as And<_T>>::Result.
}

trait Or<_T> {
    type Result.
    fn or(self, other: _T) -> <Self as Or<_T>>::Result.
}

trait Not {
    type Result.
    fn not(self) -> <Self as Not>::Result.
}

trait Negate {
    type Result.
    fn negate(self) -> <Self as Negate>::Result.
}

trait Deref {
    type Idx.
    type Result.

    fn deref(self, idx: <Self as Deref>::Idx) -> <Self as Deref>::Result.
}

trait DerefAssign {
    type Idx.
    type Value.

    fn deref_assign(
        self,
        idx: <Self as DerefAssign>::Idx,
        value: <Self as DerefAssign>::Value) -> <Self as DerefAssign>::Value.
}

trait Len {
    fn len(self) -> Int.
}

trait Call<_Args> {
    type Return.
    fn call(self, args: _Args) -> <Self as Call<_Args>>::Return.
}

trait AllocateArray {
    fn allocate_array(n: Int) -> [Self].
}

trait Into<_T> {
    fn into(self) -> _T.
}
