use std::internal::cursed::cursed_allocate_array.

export fn add_int(a: Int, b: Int) -> Int.
export fn mul_int(a: Int, b: Int) -> Int.
export fn div_int(a: Int, b: Int) -> Int.
export fn mod_int(a: Int, b: Int) -> Int.
export fn gt_int(a: Int, b: Int) -> Bool.
export fn eq_int(a: Int, b: Int) -> Bool.
export fn neg_int(a: Int) -> Int.
export fn add_string(a: String, b: String) -> String.
export fn get_char(s: String, i: Int) -> Char.
export fn eq_string(a: String, b: String) -> Bool.
export fn len_string(s: String) -> Int.

// Cursed exports bc they don't actually live in stdlib.
export fn deref_array<_T>(array: [_T], idx: Int) -> _T.
export fn deref_array_assign<_T>(array: [_T], idx: Int, value: _T) -> _T.
export fn array_len<_T>(array: [_T]) -> Int.
export fn call_fn<_Args, _Ret>(f: fn(), args: _Args) -> _Ret.
export fn call_closure<_Args, _Ret>(f: ||, args: _Args) -> _Ret.

impl Add<Int> for Int {
    type Result = Int.

    fn add(self, other: Int) -> Int {
        add_int(self, other)
    }
}

impl Subtract<Int> for Int {
    type Result = Int.

    fn sub(self, other: Int) -> Int {
        self + (-other)
    }
}

impl Multiply<Int> for Int {
    type Result = Int.

    fn mul(self, other: Int) -> Int {
        mul_int(self, other)
    }
}

impl Divide<Int> for Int {
    type Result = Int.

    fn div(self, other: Int) -> Int {
        div_int(self, other)
    }
}

impl Modulo<Int> for Int {
    type Result = Int.

    fn rem(self, other: Int) -> Int {
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

impl Negate for Int {
    type Result = Int.

    fn negate(self) -> Int {
        neg_int(self)
    }
}

impl And<Bool> for Bool {
    type Result = Bool.

    fn and(self, other: Bool) -> Bool {
        if self {
            other
        } else {
            self
        }
    }
}

impl Or<Bool> for Bool {
    type Result = Bool.

    fn or(self, other: Bool) -> Bool {
        if self {
            self
        } else {
            other
        }
    }
}

impl Not for Bool {
    type Result = Bool.

    fn not(self) -> Bool {
        if self {
            false
        } else {
            true
        }
    }
}

impl Add<String> for String {
    type Result = String.
    
    fn add(self, other: String) -> String {
        add_string(self, other)
    }
}

impl<_T> Deref for [_T] {
    type Idx = Int.
    type Result = _T.

    fn deref(self, idx: Int) -> _T {
        deref_array(self, idx)
    }
}

impl Deref for String {
    type Idx = Int.
    type Result = Char.

    fn deref(self, idx: Int) -> Char {
        get_char(self, idx)
    }
}

impl<_T> DerefAssign for [_T] {
    type Idx = Int.
    type Value = _T.

    fn deref_assign(self, idx: Int, value: _T) -> _T {
        deref_array_assign(self, idx, value)
    }
}

impl<_T> Len for [_T] {
    fn len(self) -> Int {
        array_len(self)
    }
}

impl Len for String {
    fn len(self) -> Int = len_string(self).
}

impl Equals<String> for String{
    fn eq(self, other: String) -> Bool {
        eq_string(self, other)
    }

    fn ne(self, other: String) -> Bool {
        !eq_string(self, other)
    }
}

impl<_T> AllocateArray for _T where _T: Default {
    fn allocate_array(n: Int) -> [_T] {
        let a = cursed_allocate_array:<_T>(n).

        for i in Range!Finite(0, n) {
          a[i] = <_T>:default().
        }

        a
    }
}