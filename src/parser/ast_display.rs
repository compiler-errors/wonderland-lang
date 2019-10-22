use std::fmt;

use crate::parser::ast::{AstTraitType, AstType};

impl fmt::Display for AstType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstType::Infer(id) => write!(f, "_{}", id.0),
            AstType::Int => write!(f, "Int"),
            AstType::Char => write!(f, "Char"),
            AstType::Bool => write!(f, "Bool"),
            AstType::String => write!(f, "String"),
            AstType::SelfType => write!(f, "Self"),
            AstType::Generic(id) => write!(f, "_{}", id),
            AstType::Array { ty } => write!(f, "[{}]", ty),
            AstType::Tuple { types } => write!(f, "({})", DisplayAstTypeList(types, true)),
            AstType::Object(module, generics) => write!(
                f,
                "{}{}",
                module.full_name().unwrap(),
                DisplayGenerics(generics)
            ),
            AstType::AssociatedType {
                obj_ty,
                trait_ty,
                name,
            } => {
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
                }
            }
            AstType::ElaboratedType { obj_ty, trait_ty } => {
                write!(f, "<{} as {}>", obj_ty, trait_ty)
            }
            AstType::GenericPlaceholder(id, name) => write!(f, "_{}{}", name, id.0),
            AstType::DummyGeneric(id, name) => write!(f, "_{}{}", name, id.0),
            AstType::Dummy(id) => write!(f, "_{}", id.0),
        }
    }
}

impl fmt::Display for AstTraitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AstTraitType(module, generics) = self;
        write!(
            f,
            "{}{}",
            module.full_name().unwrap(),
            DisplayGenerics(generics)
        )
    }
}

/// Wrapper type to allow displaying a comma-separated sequence of AstType.
struct DisplayAstTypeList<'a>(&'a [AstType], bool);

/// Wrapper type which conditionally displays a generics list (e.g. for Foo vs Foo<String, Int>).
struct DisplayGenerics<'a>(&'a [AstType]);

impl<'a> fmt::Display for DisplayAstTypeList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &DisplayAstTypeList(types, needs_final_comma) = self;
        for (i, ty) in types.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty)?;
        }
        if needs_final_comma && types.len() == 1 {
            write!(f, ",")?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for DisplayGenerics<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &DisplayGenerics(generics) = self;
        if generics.len() != 0 {
            write!(f, "<{}>", DisplayAstTypeList(generics, false))?;
        }
        Ok(())
    }
}
