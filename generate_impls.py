#!/usr/bin/env python3
"""
This is a helpful little script that generates the stupid traits to call closures and fns.
"""

print("""object CursedEmptyStruct {}""")

ARGFMT = """
impl<_Ret{comma_generic_tys}> Call<{args_tuple}> for {closure_type} {{
    type Return = _Ret.
    fn call(self, args: {args_tuple}) -> _Ret {{
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :{fn_type_with_env}) -> {fn_type_with_env}.
        instruction "call" (fn_ptr_cheshire, env_ptr{comma_unpacked_args}) -> _Ret
    }}
}}

impl<_Ret{comma_generic_tys}> Call<{args_tuple}> for {fn_type} {{
    type Return = _Ret.
    fn call(self, args: {args_tuple}) -> _Ret {{
        instruction "call" (self{comma_unpacked_args}) -> _Ret
    }}
}}
"""

for i in range(0,8):
    generics = ["_" + chr(ord('A') + j) for j in range(i)]
    comma_generic_tys = "" if len(generics) == 0 else (", " + ", ".join(generics))
    args_tuple = "()" if len(generics) == 0 else ("(_A,)" if len(generics) == 1 else ("(" + ", ".join(generics) + ")"))
    generic_tys = ", ".join(generics)
    closure_type = f"|{generic_tys}| -> _Ret"
    fn_type = f"fn({generic_tys}) -> _Ret"
    fn_type_with_env = "fn(" + ", ".join(["CursedEmptyStruct"] + generics) + ") -> _Ret"
    comma_unpacked_args = "" if len(generics) == 0 else (", " + ", ".join("args:" + str(i) for i in range(i)))

    print(ARGFMT.format(args_tuple=args_tuple,
                        comma_generic_tys=comma_generic_tys,
                        closure_type=closure_type,
                        fn_type=fn_type,
                        fn_type_with_env=fn_type_with_env,
                        comma_unpacked_args=comma_unpacked_args))
