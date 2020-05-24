use std::fmt;

use crate::ast::{AstTraitType, AstTraitTypeWithAssocs, AstType};
use std::collections::BTreeMap;

impl fmt::Display for AstType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstType::Infer(id) => write!(f, "_{}(i)", id.0),
            AstType::Int => write!(f, "Int"),
            AstType::Float => write!(f, "Float"),
            AstType::Char => write!(f, "Char"),
            AstType::Bool => write!(f, "Bool"),
            AstType::String => write!(f, "String"),
            AstType::SelfType => write!(f, "Self"),
            AstType::Generic(id) => write!(f, "_{}(g)", id),
            AstType::Array { ty } => write!(f, "[{}]", ty),
            AstType::Tuple { types } => write!(f, "({})", DisplayAstTypeList(types, None, true)),
            AstType::Object(module, generics)
            | AstType::Enum(module, generics)
            | AstType::ObjectEnum(module, generics) => write!(
                f,
                "{}{}",
                module.full_name(),
                DisplayGenerics(generics, None)
            ),
            AstType::ClosureType { args, ret_ty } => write!(
                f,
                "|{}| -> {}",
                DisplayAstTypeList(args, None, false),
                *ret_ty
            ),
            AstType::ClosureEnvType => write!(f, "ClosureEnvType"),
            AstType::FnPointerType { args, ret_ty } => write!(
                f,
                "fn({}) -> {}",
                DisplayAstTypeList(args, None, false),
                *ret_ty
            ),
            AstType::AssociatedType {
                obj_ty,
                trait_ty,
                name,
            } =>
                if trait_ty.is_none() {
                    write!(f, "{}::{}", obj_ty, name)
                } else {
                    write!(
                        f,
                        "<{} as {}>::{}",
                        obj_ty,
                        trait_ty.as_ref().unwrap(),
                        name
                    )
                },
            AstType::ElaboratedType { obj_ty, trait_ty } =>
                write!(f, "<{} as {}>", obj_ty, trait_ty),
            AstType::DynamicType { trait_tys } =>
                write!(f, "Dyn<{}>", DisplayTraitTypes(trait_tys.iter())),
            AstType::GenericPlaceholder(id, name) => write!(f, "_{}{}(gp)", name, id.0),
            AstType::DummyGeneric(id, name) => write!(f, "_{}{}(dg)", name, id.0),
            AstType::Dummy(id) => write!(f, "_{}(d)", id.0),
        }
    }
}

impl fmt::Display for AstTraitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AstTraitType { name, generics } = self;
        write!(f, "{}{}", name.full_name(), DisplayGenerics(generics, None))
    }
}

impl fmt::Display for AstTraitTypeWithAssocs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AstTraitTypeWithAssocs {
            trt: AstTraitType { name, generics },
            assoc_bindings,
        } = self;
        write!(
            f,
            "{}{}",
            name.full_name(),
            DisplayGenerics(generics, Some(assoc_bindings))
        )
    }
}

/// Wrapper type to allow displaying a comma-separated sequence of AstType.
struct DisplayAstTypeList<'a>(&'a [AstType], Option<&'a BTreeMap<String, AstType>>, bool);

impl<'a> fmt::Display for DisplayAstTypeList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &DisplayAstTypeList(types, assoc_bindings, needs_final_comma) = self;
        let mut printed = 0;

        for ty in types.iter() {
            if printed > 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", ty)?;
            printed += 1;
        }

        if let Some(assoc_bindings) = assoc_bindings {
            for (name, ty) in assoc_bindings {
                if printed > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}={}", name, ty)?;
                printed += 1;
            }
        }

        if needs_final_comma && printed == 1 {
            write!(f, ",")?;
        }
        Ok(())
    }
}

/// Wrapper type which conditionally displays a generics list (e.g. for Foo vs
/// Foo<String, Int>).
struct DisplayGenerics<'a>(&'a [AstType], Option<&'a BTreeMap<String, AstType>>);

impl<'a> fmt::Display for DisplayGenerics<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &DisplayGenerics(generics, assoc_bindings) = self;
        if generics.len() != 0 {
            write!(
                f,
                "<{}>",
                DisplayAstTypeList(generics, assoc_bindings, false)
            )?;
        }
        Ok(())
    }
}

pub struct DisplayTraitTypes<'a, I: Iterator<Item = &'a AstTraitTypeWithAssocs> + Clone>(pub I);

impl<'a, I> fmt::Display for DisplayTraitTypes<'a, I>
where
    I: Iterator<Item = &'a AstTraitTypeWithAssocs> + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let DisplayTraitTypes(traits) = self;

        for (idx, trt) in traits.clone().enumerate() {
            if idx > 0 {
                write!(f, " + ")?;
            }

            write!(f, "{}", trt)?;
        }

        Ok(())
    }
}
