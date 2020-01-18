export fn add_string(a: String, b: String) -> String.
export fn get_char(s: String, i: Int) -> Char.
export fn eq_string(a: String, b: String) -> Bool.

impl Add<Int> for Int {
    type Result = Int.

    fn add(self, other: Int) -> Int {
        instruction "add" (self, other) -> Int
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
        instruction "mul" (self, other) -> Int
    }
}

impl Divide<Int> for Int {
    type Result = Int.

    fn div(self, other: Int) -> Int {
        instruction "sdiv" (self, other) -> Int
    }
}

impl Modulo<Int> for Int {
    type Result = Int.

    fn rem(self, other: Int) -> Int {
        instruction "srem" (self, other) -> Int
    }
}

impl PartialCompare<Int> for Int {
    fn compare(self, other: Int) -> Int {
        self - other
    }
}

impl Subtract<Char> for Char {
    type Result = Int.

    fn sub(self, other: Char) -> Int {
        instruction "sub" (self, other) -> $tmp.
        instruction "sext" ($tmp, :Int) -> Int
    }
}

impl PartialCompare<Char> for Char {
    fn compare(self, other: Char) -> Int {
        self - other
    }
}

impl<_S, _T> Compare<_T> for _S where _S: PartialCompare<_T> {
    fn gt(self, other: _T) -> Bool {
        let res = self:compare(other).
        instruction "icmp sgt" (res, 0) -> Bool
    }

    fn lt(self, other: _T) -> Bool {
        let res = self:compare(other).
        instruction "icmp sgt" (0, res) -> Bool
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
        instruction "icmp eq" (res, 0) -> Bool
    }

    fn ne(self, other: _T) -> Bool {
        !(self == other)
    }
}

impl Negate for Int {
    type Result = Int.

    fn negate(self) -> Int {
        instruction "neg" (self) -> Int
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

impl Deref for String {
    type Idx = Int.
    type Result = Char.

    fn deref(self, idx: Int) -> Char {
        instruction "getelementptr" (self, 0, 1, idx) -> $ptr.
        instruction "load" ($ptr) -> Char
    }
}

impl Len for String {
    fn len(self) -> Int {
        instruction "getelementptr" (self, 0, 0) -> $len.
        instruction "load" ($len) -> Int
    }
}

impl Equals<String> for String{
    fn eq(self, other: String) -> Bool {
        eq_string(self, other)
    }

    fn ne(self, other: String) -> Bool {
        !eq_string(self, other)
    }
}

impl<_T> Deref for [_T] {
    type Idx = Int.
    type Result = _T.

    fn deref(self, idx: Int) -> _T {
        if idx >= self:len() {
            panic:<()>("Index out of bounds!").
        }

        instruction "getelementptr" (self, 0, 2, idx) -> $ptr.
        instruction "load" ($ptr) -> _T
    }
}

impl<_T> DerefAssign for [_T] {
    type Idx = Int.
    type Value = _T.

    fn deref_assign(self, idx: Int, value: _T) -> _T {
        if idx >= self:len() {
            panic:<()>("Index out of bounds!").
        }

        instruction "getelementptr" (self, 0, 2, idx) -> $ptr.
        instruction "store" ($ptr, value) -> ().
        value
    }
}

impl<_T> Len for [_T] {
    fn len(self) -> Int {
        instruction "getelementptr" (self, 0, 0) -> $len.
        instruction "load" ($len) -> Int
    }
}

fn allocate_empty_array<_T>(n: Int) -> [_T] {
    let ty_size = instruction "ch_typesize" (:_T) -> Int.
    instruction "ch_typeid" (:[_T]) -> $ty_id. // This is an i16, so let's store into an instruction value...
    instruction "call" ("gc_alloc_array", ty_size, n, $ty_id) -> $arr.
    instruction "pointercast" ($arr, :[_T]) -> [_T]
}

impl<_T> AllocateArray for _T where _T: Default {
    fn allocate_array(n: Int) -> [_T] {
        let a = allocate_empty_array:<_T>(n).

        for i in Range!Finite(0, n) {
          a[i] = <_T>:default().
        }

        a
    }
}