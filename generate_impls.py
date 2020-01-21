#!/usr/bin/env python3
"""
This is a helpful little script that generates the stupid traits to call closures and fns.
"""

print("""object CursedEmptyStruct {}""")

ARGFMT = """
impl<_Ret{comma_generic_tys}> Call<{tuple_ty}> for {closure_type} {{
    type Return = _Ret.
    fn call(self, args: {tuple_ty}) -> _Ret {{
        instruction "getelementptr" (self, 0, 0) -> $fn_ptr_ptr.
        instruction "load" ($fn_ptr_ptr) -> $fn_ptr.
        let env_ptr = instruction "pointercast" (self, :CursedEmptyStruct) -> CursedEmptyStruct.
        let fn_ptr_cheshire = instruction "pointercast" ($fn_ptr, :{fn_type_with_env}) -> {fn_type_with_env}.
        instruction "call" (fn_ptr_cheshire, env_ptr{comma_unpacked_args}) -> _Ret
    }}
}}

impl<_Ret{comma_generic_tys}> Call<{tuple_ty}> for {fn_type} {{
    type Return = _Ret.
    fn call(self, args: {tuple_ty}) -> _Ret {{
        instruction "call" (self{comma_unpacked_args}) -> _Ret
    }}
}}

impl{angled_generic_tys} Into<String> for {tuple_ty} {where_into_string} {{
    fn into(self) -> String {{
        {string_constructor}
    }}
}}
"""

for i in range(0,8):
    generics = ["_" + chr(ord('A') + j) for j in range(i)]
    generic_tys = ", ".join(generics)
    comma_generic_tys = "" if len(generics) == 0 else (", " + ", ".join(generics))
    tuple_ty = "()" if len(generics) == 0 else ("(_A,)" if len(generics) == 1 else ("(" + ", ".join(generics) + ")"))
    angled_generic_tys = "" if len(generics) == 0 else ("<" + ", ".join(generics) + ">")
    closure_type = f"|{generic_tys}| -> _Ret"
    fn_type = f"fn({generic_tys}) -> _Ret"
    fn_type_with_env = "fn(" + ", ".join(["CursedEmptyStruct"] + generics) + ") -> _Ret"
    comma_unpacked_args = "" if len(generics) == 0 else (", " + ", ".join("args:" + str(i) for i in range(i)))
    where_into_string = "" if len(generics) == 0 else ("where " + ", ".join(g + ": Into<String>" for g in generics))
    string_constructor = "\"()\"" if len(generics) == 0 else ("\"(\(self:0),)\"" if len(generics) == 1 else ("\"(\(" + "), \(".join("self:" + str(i) for i in range(i)) + "))\""))

    print(ARGFMT.format(tuple_ty=tuple_ty,
                        generic_tys=generic_tys,
                        comma_generic_tys=comma_generic_tys,
                        angled_generic_tys=angled_generic_tys,
                        closure_type=closure_type,
                        fn_type=fn_type,
                        fn_type_with_env=fn_type_with_env,
                        comma_unpacked_args=comma_unpacked_args,
                        where_into_string=where_into_string,
                        string_constructor=string_constructor))
