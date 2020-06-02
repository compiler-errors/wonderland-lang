// inst pass

object YieldMe<_T> {
    item: Option<_T>,
}

impl<_T> for YieldMe<_T> {
    fn new(item: Option<_T>) -> YieldMe<_T> {
        allocate YieldMe { item }
    }
    
    fn put(self, t: _T) {
        self:item = Option!Some(t).
    }
}

enum YieldMeIterator<_T> {
    Iterator(YieldMe<_T>),
}

impl<_T> Iterable for YieldMe<_T> {
    type Iterator = YieldMeIterator<_T>.
    type Item = _T.

    fn iterator(self) -> YieldMeIterator<_T> = 
        YieldMeIterator!Iterator(self).
}

impl<_T> Iterator for YieldMeIterator<_T> {
    type Item = _T.

    fn next(self) -> (Option<_T>, YieldMeIterator<_T>) {
        let YieldMeIterator!Iterator(y) = self.

        if let Option!Some(i) = y:item {
            y:item = Option!None.
            (Option!Some(i), self)
        } else {
            (Option!None, self)
        }
    }

    fn has_next(self) -> Bool {
        let YieldMeIterator!Iterator(y) = self.
        y:item:is_some()        
    }

    fn size_hint(self) -> Int {
        let YieldMeIterator!Iterator(y) = self.
        
        if y:item:is_some() {
            1
        } else {
            0
        }
    }
}

fn main() -> Int {
    let a = YieldMe:new(Option!Some(0)).
    let b = YieldMe:new(Option!Some(1)).

    for (i, (ax, bx)) in a:iterator():zip(b:iterator()):enumerate() {
        println("fib(\(i)) = \(ax)").
        
        a:put(bx).
        b:put(ax + bx).
    }
    
    0
}

