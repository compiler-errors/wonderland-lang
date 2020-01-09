
use std::internal::operators::call_closure.
use std::internal::operators::call_fn.
use std::internal::cursed::cursed_transmute.

impl<_Ret> Call<()> for || -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: ()) -> _Ret {
        let f = cursed_transmute:<|| -> _Ret, ||>(self).
        call_closure:<(), _Ret>(f, ())
    }
}

impl<_Ret> Call<()> for fn() -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: ()) -> _Ret {
        let f = cursed_transmute:<fn() -> _Ret, fn()>(self).
        call_fn:<(), _Ret>(f, ())
    }
}


impl<_Ret, _A> Call<(_A,)> for |_A| -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A,)) -> _Ret {
        let f = cursed_transmute:<|_A| -> _Ret, ||>(self).
        call_closure:<(_A,), _Ret>(f, args)
    }
}

impl<_Ret, _A> Call<(_A,)> for fn(_A) -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A,)) -> _Ret {
        let f = cursed_transmute:<fn(_A) -> _Ret, fn()>(self).
        call_fn:<(_A,), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B> Call<(_A, _B)> for |_A, _B| -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B)) -> _Ret {
        let f = cursed_transmute:<|_A, _B| -> _Ret, ||>(self).
        call_closure:<(_A, _B), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B> Call<(_A, _B)> for fn(_A, _B) -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B) -> _Ret, fn()>(self).
        call_fn:<(_A, _B), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C> Call<(_A, _B, _C)> for |_A, _B, _C| -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C> Call<(_A, _B, _C)> for fn(_A, _B, _C) -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D> Call<(_A, _B, _C, _D)> for |_A, _B, _C, _D| -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D> Call<(_A, _B, _C, _D)> for fn(_A, _B, _C, _D) -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D, _E> Call<(_A, _B, _C, _D, _E)> for |_A, _B, _C, _D, _E| -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D, _E| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D, _E), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D, _E> Call<(_A, _B, _C, _D, _E)> for fn(_A, _B, _C, _D, _E) -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D, _E) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D, _E), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D, _E, _F> Call<(_A, _B, _C, _D, _E, _F)> for |_A, _B, _C, _D, _E, _F| -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D, _E, _F| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D, _E, _F), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D, _E, _F> Call<(_A, _B, _C, _D, _E, _F)> for fn(_A, _B, _C, _D, _E, _F) -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D, _E, _F) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D, _E, _F), _Ret>(f, args)
    }
}


impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Call<(_A, _B, _C, _D, _E, _F, _G)> for |_A, _B, _C, _D, _E, _F, _G| -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret {
        let f = cursed_transmute:<|_A, _B, _C, _D, _E, _F, _G| -> _Ret, ||>(self).
        call_closure:<(_A, _B, _C, _D, _E, _F, _G), _Ret>(f, args)
    }
}

impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Call<(_A, _B, _C, _D, _E, _F, _G)> for fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret {
    type CallReturn = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret {
        let f = cursed_transmute:<fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret, fn()>(self).
        call_fn:<(_A, _B, _C, _D, _E, _F, _G), _Ret>(f, args)
    }
}

