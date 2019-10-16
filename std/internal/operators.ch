export fn add_int(a: Int, b: Int) -> Int.
export fn sub_int(a: Int, b: Int) -> Int.
export fn mul_int(a: Int, b: Int) -> Int.
export fn div_int(a: Int, b: Int) -> Int.
export fn mod_int(a: Int, b: Int) -> Int.
export fn gt_int(a: Int, b: Int) -> Bool.
export fn eq_int(a: Int, b: Int) -> Bool.
export fn add_string(a: String, b: String) -> String.

impl Add<Int> for Int {
    type AddResult = Int.

    fn add(self, other: Int) -> Int {
        add_int(self, other)
    }
}

impl Subtract<Int> for Int {
    type SubtractResult = Int.

    fn sub(self, other: Int) -> Int {
        sub_int(self, other)
    }
}

impl Multiply<Int> for Int {
    type MultiplyResult = Int.

    fn mul(self, other: Int) -> Int {
        mul_int(self, other)
    }
}

impl Divide<Int> for Int {
    type DivideResult = Int.

    fn div(self, other: Int) -> Int {
        div_int(self, other)
    }
}

impl Modulo<Int> for Int {
    type ModuloResult = Int.

    fn rem(self, other: Int) -> Self::ModuloResult {
        mod_int(self, other)
    }
}

impl PartialCompare<Int> for Int {
    fn compare(self, other: Int) -> Int {
        self - other
    }
}

impl<_S, _T> Compare<_T> for _S where _S: PartialCompare<_T> {
    fn gt(self, other: _T) -> Bool {
        let res = self:compare(other).
        gt_int(res, 0)
    }

    fn lt(self, other: _T) -> Bool {
        let res = self:compare(other).
        gt_int(0, res)
    }

    fn ge(self, other: _T) -> Bool {
        !(self < other)
    }

    fn le(self, other: _T) -> Bool {
        !(self > other)
    }
}

impl<_S, _T> Equals<_T> for _S where _S: PartialCompare<_T> {
    fn eq(self, other: _T) -> Bool {
        let res = self:compare(other).
        eq_int(res, 0)
    }

    fn ne(self, other: _T) -> Bool {
        !(self == other)
    }
}

impl And<Bool> for Bool {
    type AndResult = Bool.

    fn and(self, other: Bool) -> Bool {
        if self {
            other
        } else {
            self
        }
    }
}

impl Or<Bool> for Bool {
    type OrResult = Bool.

    fn or(self, other: Bool) -> Bool {
        if self {
            self
        } else {
            other
        }
    }
}

impl Add<String> for String {
    type AddResult = String.
    
    fn add(self, other: String) -> String {
        add_string(self, other)
    }
}