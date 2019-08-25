use crate::parser::ast::*;
use crate::parser::ast_visitor::Adapter;
use crate::util::result::*;
use crate::util::Span;

pub struct SelfAdapter {
    current_trait: Option<AstTraitType>,
    current_impl: Option<AstType>,
}

impl SelfAdapter {
    pub fn new() -> SelfAdapter {
        SelfAdapter {
            current_trait: None,
            current_impl: None,
        }
    }
}

impl Adapter for SelfAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match (&self.current_impl, &self.current_trait, t) {
            (&Some(ref t), _, AstType::SelfType) => Ok(t.clone()),
            (&None, &None, AstType::SelfType) => PError::new(
                Span::new(0, 0),
                format!("Self type in non-self environment"),
            ),
            (_, _, t) => Ok(t),
        }
    }

    fn enter_trait(&mut self, mut t: AstTrait) -> PResult<AstTrait> {
        if self.current_trait.is_some() {
            PError::new(
                Span::new(0, 0),
                format!("Trait not expected in trait context..."),
            )?;
        }

        let name = t.name.clone();
        let generics = t.generics.iter().map(|g| g.clone().into()).collect();
        self.current_trait = Some(AstTraitType(name, generics));

        t.associated_types
            .insert("Self".to_string(), AstAssociatedType::self_ty());

        Ok(t)
    }

    fn enter_impl(&mut self, mut i: AstImpl) -> PResult<AstImpl> {
        if self.current_trait.is_some() {
            PError::new(
                Span::new(0, 0),
                format!("Trait not expected in trait context..."),
            )?;
        }

        self.current_trait = Some(i.trait_ty.clone());
        self.current_impl = Some(i.impl_ty.clone());

        i.associated_types
            .insert("Self".to_string(), i.impl_ty.clone());

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

    // TODO: Self type shouldn't be legal in various places.
}
