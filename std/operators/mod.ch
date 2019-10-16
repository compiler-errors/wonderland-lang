trait Add<_T> {
    type AddResult.
    fn add(self, other: _T) -> Self::AddResult.
}

trait Subtract<_T> {
    type SubtractResult.
    fn sub(self, other: _T) -> Self::SubtractResult.
}

trait Multiply<_T> {
    type MultiplyResult.
    fn mul(self, other: _T) -> Self::MultiplyResult.
}

trait Divide<_T> {
    type DivideResult.
    fn div(self, other: _T) -> Self::DivideResult.
}

trait Modulo<_T> {
    type ModuloResult.
    fn rem(self, other: _T) -> Self::ModuloResult.
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
    fn and(self, other: _T) -> Self::AndResult.
}

trait Or<_T> {
    type OrResult.
    fn or(self, other: _T) -> Self::OrResult.
}