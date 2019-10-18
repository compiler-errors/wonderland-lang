trait Add<_T> {
    type AddResult.
    fn add(self, other: _T) -> <Self as Add<_T>>::AddResult.
}

trait Subtract<_T> {
    type SubtractResult.
    fn sub(self, other: _T) -> <Self as Subtract<_T>>::SubtractResult.
}

trait Multiply<_T> {
    type MultiplyResult.
    fn mul(self, other: _T) -> <Self as Multiply<_T>>::MultiplyResult.
}

trait Divide<_T> {
    type DivideResult.
    fn div(self, other: _T) -> <Self as Divide<_T>>::DivideResult.
}

trait Modulo<_T> {
    type ModuloResult.
    fn rem(self, other: _T) -> <Self as Modulo<_T>>::ModuloResult.
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

trait Equals<_T> {
    fn eq(self, other: _T) -> Bool.
    fn ne(self, other: _T) -> Bool.
}

trait And<_T> {
    type AndResult.
    fn and(self, other: _T) -> <Self as And<_T>>::AndResult.
}

trait Or<_T> {
    type OrResult.
    fn or(self, other: _T) -> <Self as Or<_T>>::OrResult.
}

trait Not {
    type NotResult.
    fn not(self) -> <Self as Not>::NotResult.
}

trait Negate {
    type NegateResult.
    fn negate(self) -> <Self as Negate>::NegateResult.
}

trait Deref<_Idx> {
    type DerefResult.

    fn deref(self, idx: _Idx) -> <Self as Deref<_Idx>>::DerefResult.
}

trait Len {
    fn len(self) -> Int.
}