use crate::parser::ast::*;
use crate::util::{FileRegistry, PResult};

fn decorate_mod_name(m: &ModuleRef) -> PResult<String> {
    if let ModuleRef::Normalized(file, name) = m {
        Ok(format!(
            "{}.{}",
            FileRegistry::mod_path(*file)?.join("."),
            name
        ))
    } else {
        unreachable!()
    }
}

pub fn decorate_ty(t: &AstType) -> PResult<String> {
    Ok(match t {
        AstType::Int => "I".into(),
        AstType::Char => "H".into(), /* TODO: `C` and `c` are reserved for closures. */
        AstType::Bool => "B".into(),
        AstType::String => "N".into(),

        AstType::Array { ty } => format!("A{}", decorate_ty(&ty)?),

        AstType::Tuple { types } if types.len() == 0 => "U".into(),

        AstType::Tuple { types } if types.len() == 1 => format!("t{}", decorate_ty(&types[0])?),

        AstType::Tuple { types } => {
            let mut string = format!("T{}", types.len());

            for t in types {
                string.push_str(&decorate_ty(t)?);
            }

            string
        }

        AstType::Object(name, generics) if generics.len() == 0 => {
            let name = decorate_mod_name(name)?;
            format!("o{}{}", name.len(), name)
        }

        AstType::Object(name, generics) => {
            let name = decorate_mod_name(name)?;
            let mut string = format!("O{}{}{}", name.len(), name, generics.len());

            for t in generics {
                string.push_str(&decorate_ty(t)?);
            }

            string
        }

        _ => unreachable!(),
    })
}

pub fn decorate_fn(name: &ModuleRef, generics: &[AstType]) -> PResult<String> {
    let name = decorate_mod_name(name)?;

    Ok(if generics.len() == 0 {
        format!("f{}{}", name.len(), name)
    } else {
        let mut string = format!("F{}{}{}", name.len(), name, generics.len());

        for t in generics {
            string.push_str(&decorate_ty(t)?);
        }

        string
    })
}

pub fn decorate_object(name: &ModuleRef, generics: &[AstType]) -> PResult<String> {
    let name = decorate_mod_name(name)?;

    Ok(if generics.len() == 0 {
        format!("s{}{}", name.len(), name)
    } else {
        let mut string = format!("S{}{}{}", name.len(), name, generics.len());

        for t in generics {
            string.push_str(&decorate_ty(t)?);
        }

        string
    })
}

pub fn decorate_object_fn(
    ty: &AstType,
    trt: &AstTraitType,
    fn_name: &str,
    fn_generics: &[AstType],
) -> PResult<String> {
    let trt_name = decorate_mod_name(&trt.0)?;

    Ok(if trt.1.len() == 0 && fn_generics.len() == 0 {
        format!(
            "i{}{}{}{}{}",
            decorate_ty(ty)?,
            trt_name.len(),
            trt_name,
            fn_name.len(),
            fn_name
        )
    } else if fn_generics.len() == 0 {
        let mut string = format!(
            "m{}{}{}{}",
            decorate_ty(ty)?,
            trt_name.len(),
            trt_name,
            trt.1.len()
        );

        for t in &trt.1 {
            string.push_str(&decorate_ty(t)?);
        }

        string.push_str(&format!("{}{}", fn_name.len(), fn_name));

        string
    } else {
        let mut string = format!(
            "M{}{}{}{}",
            decorate_ty(ty)?,
            trt_name.len(),
            trt_name,
            trt.1.len()
        );

        for t in &trt.1 {
            string.push_str(&decorate_ty(t)?);
        }

        string.push_str(&format!(
            "_{}{}{}",
            fn_name.len(),
            fn_name,
            fn_generics.len()
        ));

        for t in fn_generics {
            string.push_str(&decorate_ty(t)?);
        }

        string
    })
}
