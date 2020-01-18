object CursedEmptyStruct {}

impl<_Ret> Call<()> for || -> _Ret {
    type Return = _Ret.
    fn call(self, args: ()) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct) -> _Ret) -> fn(CursedEmptyStruct) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr) -> _Ret
    }
}

impl<_Ret> Call<()> for fn() -> _Ret {
    type Return = _Ret.
    fn call(self, args: ()) -> _Ret {
        instruction "call" (self) -> _Ret
    }
}


impl<_Ret, _A> Call<(_A,)> for |_A| -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A,)) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct, _A) -> _Ret) -> fn(CursedEmptyStruct, _A) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr, args:0) -> _Ret
    }
}

impl<_Ret, _A> Call<(_A,)> for fn(_A) -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A,)) -> _Ret {
        instruction "call" (self, args:0) -> _Ret
    }
}


impl<_Ret, _A, _B> Call<(_A, _B)> for |_A, _B| -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B)) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct, _A, _B) -> _Ret) -> fn(CursedEmptyStruct, _A, _B) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr, args:0, args:1) -> _Ret
    }
}

impl<_Ret, _A, _B> Call<(_A, _B)> for fn(_A, _B) -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B)) -> _Ret {
        instruction "call" (self, args:0, args:1) -> _Ret
    }
}


impl<_Ret, _A, _B, _C> Call<(_A, _B, _C)> for |_A, _B, _C| -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C)) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct, _A, _B, _C) -> _Ret) -> fn(CursedEmptyStruct, _A, _B, _C) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr, args:0, args:1, args:2) -> _Ret
    }
}

impl<_Ret, _A, _B, _C> Call<(_A, _B, _C)> for fn(_A, _B, _C) -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C)) -> _Ret {
        instruction "call" (self, args:0, args:1, args:2) -> _Ret
    }
}


impl<_Ret, _A, _B, _C, _D> Call<(_A, _B, _C, _D)> for |_A, _B, _C, _D| -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D)) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct, _A, _B, _C, _D) -> _Ret) -> fn(CursedEmptyStruct, _A, _B, _C, _D) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr, args:0, args:1, args:2, args:3) -> _Ret
    }
}

impl<_Ret, _A, _B, _C, _D> Call<(_A, _B, _C, _D)> for fn(_A, _B, _C, _D) -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D)) -> _Ret {
        instruction "call" (self, args:0, args:1, args:2, args:3) -> _Ret
    }
}


impl<_Ret, _A, _B, _C, _D, _E> Call<(_A, _B, _C, _D, _E)> for |_A, _B, _C, _D, _E| -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct, _A, _B, _C, _D, _E) -> _Ret) -> fn(CursedEmptyStruct, _A, _B, _C, _D, _E) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr, args:0, args:1, args:2, args:3, args:4) -> _Ret
    }
}

impl<_Ret, _A, _B, _C, _D, _E> Call<(_A, _B, _C, _D, _E)> for fn(_A, _B, _C, _D, _E) -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret {
        instruction "call" (self, args:0, args:1, args:2, args:3, args:4) -> _Ret
    }
}


impl<_Ret, _A, _B, _C, _D, _E, _F> Call<(_A, _B, _C, _D, _E, _F)> for |_A, _B, _C, _D, _E, _F| -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct, _A, _B, _C, _D, _E, _F) -> _Ret) -> fn(CursedEmptyStruct, _A, _B, _C, _D, _E, _F) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr, args:0, args:1, args:2, args:3, args:4, args:5) -> _Ret
    }
}

impl<_Ret, _A, _B, _C, _D, _E, _F> Call<(_A, _B, _C, _D, _E, _F)> for fn(_A, _B, _C, _D, _E, _F) -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret {
        instruction "call" (self, args:0, args:1, args:2, args:3, args:4, args:5) -> _Ret
    }
}


impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Call<(_A, _B, _C, _D, _E, _F, _G)> for |_A, _B, _C, _D, _E, _F, _G| -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret {
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :fn(CursedEmptyStruct, _A, _B, _C, _D, _E, _F, _G) -> _Ret) -> fn(CursedEmptyStruct, _A, _B, _C, _D, _E, _F, _G) -> _Ret.
        instruction "call" (fn_ptr_cheshire, env_ptr, args:0, args:1, args:2, args:3, args:4, args:5, args:6) -> _Ret
    }
}

impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Call<(_A, _B, _C, _D, _E, _F, _G)> for fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret {
    type Return = _Ret.
    fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret {
        instruction "call" (self, args:0, args:1, args:2, args:3, args:4, args:5, args:6) -> _Ret
    }
}

