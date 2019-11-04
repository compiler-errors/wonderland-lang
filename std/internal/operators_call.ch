
use std::internal::operators::call_closure.
use std::internal::operators::call_fn.
use std::internal::cursed::cursed_transmute.

impl<_Ret> Fn() -> _Ret for || -> _Ret {
    fn call(self, args: ()) -> _Ret {
        let f = cursed_transmute:<|| -> _Ret, ||>(self).
        call_closure:<(), _Ret>(f, ())
    }
}

impl<_Ret> Fn() -> _Ret for fn() -> _Ret {
    fn call(self, args: ()) -> _Ret {
        let f = cursed_transmute:<fn() -> _Ret, fn()>(self).
        call_fn:<(), _Ret>(f, ())
    }
}


impl<_Ret, _A> Fn(_A) -> _Ret for |_A| -> _Ret {
    fn call(self, args: (_A,)) -> _Ret {
        let f = cursed_transmute:<|_A| -> _Ret, ||>(self).
        call_closure:<(_A,), _Ret>(f, args)
    }
}

impl<_Ret, _A> Fn(_A) -> _Ret for fn(_A) -> _Ret {
    fn call(self, args: (_A,)) -> _Ret {
        let f = cursed_transmute:<fn(_A) -> _Ret, fn()>(self).
        call_fn:<(_A,), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B> Fn(_A, _B) -> _Ret for |_A, _B| -> _Ret {
    fn call(self, args: (_A, _B)) -> _Ret {
        let f = cursed_transmute:<|_A, _B| -> _Ret, ||>(self).
        call_closure:<(_A, _B), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B> Fn(_A, _B) -> _Ret for fn(_A, _B) -> _Ret {
    fn call(self, args: (_A, _B)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B) -> _Ret, fn()>(self).
        call_fn:<(_A, _B), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C> Fn(_A, _B, _C) -> _Ret for |_A, _B, _C| -> _Ret {
    fn call(self, args: (_A, _B, _C)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C> Fn(_A, _B, _C) -> _Ret for fn(_A, _B, _C) -> _Ret {
    fn call(self, args: (_A, _B, _C)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D> Fn(_A, _B, _C, _D) -> _Ret for |_A, _B, _C, _D| -> _Ret {
    fn call(self, args: (_A, _B, _C, _D)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D> Fn(_A, _B, _C, _D) -> _Ret for fn(_A, _B, _C, _D) -> _Ret {
    fn call(self, args: (_A, _B, _C, _D)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D, _E> Fn(_A, _B, _C, _D, _E) -> _Ret for |_A, _B, _C, _D, _E| -> _Ret {
    fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D, _E| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D, _E), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D, _E> Fn(_A, _B, _C, _D, _E) -> _Ret for fn(_A, _B, _C, _D, _E) -> _Ret {
    fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D, _E) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D, _E), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D, _E, _F> Fn(_A, _B, _C, _D, _E, _F) -> _Ret for |_A, _B, _C, _D, _E, _F| -> _Ret {
    fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D, _E, _F| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D, _E, _F), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D, _E, _F> Fn(_A, _B, _C, _D, _E, _F) -> _Ret for fn(_A, _B, _C, _D, _E, _F) -> _Ret {
    fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D, _E, _F) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D, _E, _F), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret for |_A, _B, _C, _D, _E, _F, _G| -> _Ret {
    fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D, _E, _F, _G| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D, _E, _F, _G), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret for fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret {
    fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D, _E, _F, _G), _Ret>(f, args)
    }
}

