#!/usr/bin/env python3
"""
This is a helpful little method that generates the stupid traits to call closures and fns.
"""

print("""
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
""")

ARGFMT = """
impl<_Ret, {generics}> Fn({generics}) -> _Ret for |{generics}| -> _Ret {{
    fn call(self, args: ({args})) -> _Ret {{
        let f = cursed_transmute:<|{generics}| -> _Ret, ||>(self).
        call_closure:<({args}), _Ret>(f, args)
    }}
}}

impl<_Ret, {generics}> Fn({generics}) -> _Ret for fn({generics}) -> _Ret {{
    fn call(self, args: ({args})) -> _Ret {{
        let f = cursed_transmute:<fn({generics}) -> _Ret, fn()>(self).
        call_fn:<({args}), _Ret>(f, args)
    }}
}}
"""

for i in range(1,8):
    generics = ", ".join("_" + chr(ord('A') + j) for j in range(i))
    args = generics + ("," if i == 1 else "")
    print(ARGFMT.format(generics=generics, args=args))
