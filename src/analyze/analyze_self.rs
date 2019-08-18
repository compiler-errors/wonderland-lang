use crate::parser::*;
use crate::util::result::{PError, PResult};
use crate::util::Span;

pub struct SelfAdapter {
    current_trait: Option<AstTraitType>,
}

impl SelfAdapter {
    pub fn new() -> SelfAdapter {
        SelfAdapter {
            current_trait: None,
        }
    }
}

impl Adapter for SelfAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match (&self.current_trait, t) {
            (&None, AstType::SelfType) => PError::new(
                Span::new(0, 0),
                format!("Self type in non-self environment"),
            ),
            (&Some(ref current_trait), AstType::SelfType) => Ok(AstType::AssociatedType {
                obj_ty: Box::new(AstType::infer()),
                trait_ty: Some(current_trait.clone()),
                name: "Self".to_string(),
            }),
            (_, t) => Ok(t),
        }
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        if self.current_trait.is_some() {
            PError::new(
                Span::new(0, 0),
                format!("Trait not expected in trait context..."),
            )?;
        }

        let name = t.name.clone();
        let generics = t
            .generics
            .iter()
            .map(|g| AstType::generic(g.clone()))
            .collect();
        self.current_trait = Some(AstTraitType(name, generics));

        Ok(t)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        if self.current_trait.is_some() {
            PError::new(
                Span::new(0, 0),
                format!("Trait not expected in trait context..."),
            )?;
        }

        self.current_trait = Some(i.trait_ty.clone());

        Ok(i)
    }

    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.current_trait = None;

        Ok(t)
    }

    fn exit_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.current_trait = None;

        Ok(i)
    }
}
