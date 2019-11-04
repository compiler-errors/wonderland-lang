// inst pass

fn main() -> Int {
    let x: || -> Int = || 5.
    x()
}

impl<_R> Fn() -> _R for || -> _R {
    fn call(self, args: ()) -> _R {
        let f = cursed_transmute:<_, ||>(self).
        call_closure:<(), _R>(f, ())
    }
}