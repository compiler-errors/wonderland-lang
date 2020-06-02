extern crate heck;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use heck::SnakeCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse2, Attribute, Expr, Fields, Ident, ItemEnum, ItemStruct, Lit, Meta, NestedMeta, Path,
};

#[proc_macro_attribute]
pub fn adapter(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_derive(Visit, attributes(Adapter))]
pub fn derive_visit(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    if let Ok(st) = syn::parse::<ItemStruct>(item.clone()) {
        let ty_name = &st.ident;
        let adapter_name = get_adapter_name(&st.attrs);

        let ty_string = st.ident.to_string();
        let enter_ty_fn = format_ident!("enter_{}", ty_string.to_snake_case());
        let exit_ty_fn = format_ident!("exit_{}", ty_string.to_snake_case());

        let deconstruct_self = get_deconstruct_self(&st.ident, &st.fields);
        let reconstruct_self = get_reconstruct_self(&st.ident, &st.fields);

        let imp = quote! {
            impl<A> ::wonderland::util::Visit<A> for #ty_name where A: #adapter_name {
                fn visit(self, adapter: &mut A) -> ::wonderland::util::PResult<Self> {
                    let #deconstruct_self = <A as #adapter_name >:: #enter_ty_fn (adapter, self)?;

                    let i = #reconstruct_self;

                    <A as #adapter_name >:: #exit_ty_fn(adapter, i)
                }
            }
        };

        proc_macro::TokenStream::from(imp)
    } else if let Ok(en) = syn::parse::<ItemEnum>(item.clone()) {
        let ty_name = &en.ident;
        let adapter_name = get_adapter_name(&en.attrs);

        let ty_string = en.ident.to_string();
        let enter_ty_fn = format_ident!("enter_{}", ty_string.to_snake_case());
        let exit_ty_fn = format_ident!("exit_{}", ty_string.to_snake_case());

        let deconstruct_patterns: Vec<TokenStream> = en
            .variants
            .iter()
            .map(|v| {
                let var_name = &v.ident;
                get_deconstruct_self(&quote!(#ty_name :: #var_name), &v.fields)
            })
            .collect();

        let reconstruct_patterns: Vec<TokenStream> = en
            .variants
            .iter()
            .map(|v| {
                let var_name = &v.ident;
                get_reconstruct_self(&quote!(#ty_name :: #var_name), &v.fields)
            })
            .collect();

        let imp = quote! {
            impl<A> ::wonderland::util::Visit<A> for #ty_name where A: #adapter_name {
                fn visit(self, adapter: &mut A) -> ::wonderland::util::PResult<Self> {
                    let i = <A as #adapter_name >:: #enter_ty_fn (adapter, self)?;

                    let i = match i {
                        #(#deconstruct_patterns => #reconstruct_patterns),*
                    };

                    <A as #adapter_name >:: #exit_ty_fn(adapter, i)
                }
            }
        };

        proc_macro::TokenStream::from(imp)
    } else {
        quote!(compile_error!(
            "Expected `struct` or `enum` for derive(Visit)"
        ))
        .into()
    }
}

#[proc_macro_derive(VisitAnonymous, attributes(Adapter))]
pub fn derive_visit_anonymous(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    if let Ok(st) = syn::parse::<ItemStruct>(item.clone()) {
        let ty_name = &st.ident;
        let adapter_name = get_adapter_name(&st.attrs);

        let deconstruct_self = get_deconstruct_self(&st.ident, &st.fields);
        let reconstruct_self = get_reconstruct_self(&st.ident, &st.fields);

        let imp = quote! {
            impl<A> ::wonderland::util::Visit<A> for #ty_name where A: #adapter_name {
                fn visit(self, adapter: &mut A) -> ::wonderland::util::PResult<Self> {
                    let #deconstruct_self = self;

                    let i = #reconstruct_self;

                    Ok(i)
                }
            }
        };

        proc_macro::TokenStream::from(imp)
    } else if let Ok(en) = syn::parse::<ItemEnum>(item.clone()) {
        let ty_name = &en.ident;
        let adapter_name = get_adapter_name(&en.attrs);

        let deconstruct_patterns: Vec<TokenStream> = en
            .variants
            .iter()
            .map(|v| {
                let var_name = &v.ident;
                get_deconstruct_self(&quote!(#ty_name :: #var_name), &v.fields)
            })
            .collect();

        let reconstruct_patterns: Vec<TokenStream> = en
            .variants
            .iter()
            .map(|v| {
                let var_name = &v.ident;
                get_reconstruct_self(&quote!(#ty_name :: #var_name), &v.fields)
            })
            .collect();

        let imp = quote! {
            impl<A> ::wonderland::util::Visit<A> for #ty_name where A: #adapter_name {
                fn visit(self, adapter: &mut A) -> ::wonderland::util::PResult<Self> {
                    let i = match self {
                        #(#deconstruct_patterns => #reconstruct_patterns),*
                    };

                    Ok(i)
                }
            }
        };

        proc_macro::TokenStream::from(imp)
    } else {
        quote!(compile_error!(
            "Expected `struct` or `enum` for derive(Visit)"
        ))
        .into()
    }
}

fn get_adapter_name(attrs: &[Attribute]) -> Path {
    let mut adapter_name = None;

    for a in attrs {
        match Attribute::parse_meta(a) {
            Ok(Meta::List(list)) =>
                if list.path.get_ident() == Some(&format_ident!("Adapter")) {
                    if adapter_name.is_some() {
                        unreachable!("ICE: Conflicting adapter names in derive(Visit)");
                    }

                    if list.nested.len() == 1 {
                        if let NestedMeta::Lit(Lit::Str(name)) = list.nested.first().unwrap() {
                            adapter_name = Some(name.parse::<Path>().unwrap_or_else(|_| {
                                unreachable!(
                                    "ICE: Could not parse a Path out of \"{}\"",
                                    name.to_token_stream().to_string()
                                )
                            }));
                        } else {
                            unreachable!("ICE: Expected a string literal for Adapter")
                        }
                    } else {
                        unreachable!("ICE: Expected one literal argument for Adapter")
                    }
                },
            _ => {},
        }
    }

    adapter_name.unwrap_or_else(|| unreachable!("ICE: No associated Adapter macro found for derive(Visit)"))
}

fn get_deconstruct_self<T: ToTokens>(ty: &T, fields: &Fields) -> TokenStream {
    match fields {
        Fields::Named(fields) => {
            let vars: Vec<Ident> = fields
                .named
                .iter()
                .map(|f| f.ident.clone().unwrap())
                .collect();

            quote!(
                #ty {
                    #(#vars),*
                }
            )
            .into()
        },
        Fields::Unnamed(fields) => {
            let vars: Vec<Ident> = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(idx, _)| {
                    let id = format_ident!("__field{}", idx);
                    id
                })
                .collect();

            quote!(
                #ty (
                    #(#vars),*
                )
            )
            .into()
        },
        Fields::Unit => quote!(#ty).into(),
    }
}

fn get_reconstruct_self<T: ToTokens>(ty: &T, fields: &Fields) -> TokenStream {
    match fields {
        Fields::Named(fields) => {
            let vars: Vec<Ident> = fields
                .named
                .iter()
                .map(|f| f.ident.clone().unwrap())
                .collect();

            let vars_visited: Vec<Expr> = fields
                .named
                .iter()
                .map(|f| {
                    let id = f.ident.as_ref().unwrap();
                    let id_ty = &f.ty;
                    let st: TokenStream =
                        quote!(<#id_ty as ::wonderland::util::Visit<A>>::visit(#id, adapter)?)
                            .into();
                    parse2::<Expr>(st).unwrap()
                })
                .collect();

            quote!(
                #ty {
                    #(#vars : #vars_visited),*
                }
            )
            .into()
        },
        Fields::Unnamed(fields) => {
            let vars_visited: Vec<Expr> = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(idx, f)| {
                    let id = format_ident!("__field{}", idx);
                    let id_ty = &f.ty;
                    let st: TokenStream =
                        quote!(<#id_ty as ::wonderland::util::Visit<A>>::visit(#id, adapter)?)
                            .into();
                    parse2::<Expr>(st).unwrap()
                })
                .collect();

            quote!(
                #ty (
                    #(#vars_visited),*
                )
            )
            .into()
        },
        Fields::Unit => quote!(#ty).into(),
    }
}
