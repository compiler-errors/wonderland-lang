use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    parser::{
        ast::{AstExpression, AstExpressionData, AstFunction, AstImpl, AstObjectFunction, AstType},
        ast_visitor::AstAdapter,
    },
    util::{PResult, Visit},
};

pub struct AnalyzeAssociatedTypesAndMethods {
    program: AnalyzedProgram,
    inside_method: bool,
}

impl PureAnalysisPass for AnalyzeAssociatedTypesAndMethods {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzeAssociatedTypesAndMethods> {
        Ok(AnalyzeAssociatedTypesAndMethods {
            program: a,
            inside_method: false,
        })
    }

    fn drop(self) -> AnalyzedProgram {
        self.program
    }
}

impl AstAdapter for AnalyzeAssociatedTypesAndMethods {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.inside_method = true;

        Ok(f)
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::AssociatedType {
                trait_ty: Some(trait_ty),
                name,
                ..
            } => {
                if !self.program.analyzed_traits[&trait_ty.trt.name]
                    .associated_tys
                    .contains_key(name)
                {
                    return perror!(
                        "The given associated type `{}` does not exist in the trait",
                        t
                    );
                }
            },
            AstType::AssociatedType {
                trait_ty: None,
                name,
                ..
            } => {
                if !self.inside_method {
                    return perror!(
                        "In type `{}`, must fully specify trait type outside of method",
                        t
                    );
                }

                if !self.program.associated_types_to_traits.contains_key(name) {
                    return perror!(
                        "There are no traits with the given associated type `{}`",
                        name
                    );
                }
            },
            _ => {},
        }

        Ok(t)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                mut args,
            } => {
                let in_trait = self.program.methods_to_traits.contains_key(&fn_name);
                let in_impl = self
                    .program
                    .methods_to_anonymous_impls
                    .contains_key(&fn_name);

                if !in_trait && !in_impl {
                    return perror!(
                        "There are no traits or impls with the given method `{}`",
                        fn_name
                    );
                }

                let call_type = object.ty.clone();
                args.insert(0, *object);

                AstExpressionData::StaticCall {
                    call_type,
                    fn_name,
                    fn_generics: generics,
                    args,
                    associated_trait: None,
                    impl_signature: None,
                }
            },
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                impl_signature,
            } => {
                if let Some(associated_trait) = &associated_trait {
                    if !self.program.analyzed_traits[&associated_trait.trt.name]
                        .methods
                        .contains_key(&fn_name)
                    {
                        return perror_at!(
                            span,
                            "The given method `<{} as {}>:{}(...)` does not exist in the trait",
                            call_type,
                            associated_trait,
                            fn_name
                        );
                    }
                } else {
                    let in_trait = self.program.methods_to_traits.contains_key(&fn_name);
                    let in_impl = self
                        .program
                        .methods_to_anonymous_impls
                        .contains_key(&fn_name);

                    if !in_trait && !in_impl {
                        return perror_at!(
                            span,
                            "There are no traits or impls with the given method `{}`",
                            fn_name
                        );
                    }
                }

                AstExpressionData::StaticCall {
                    call_type,
                    fn_name,
                    fn_generics,
                    args,
                    associated_trait,
                    impl_signature,
                }
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.inside_method = true;

        Ok(o)
    }

    fn enter_impl(&mut self, mut i: AstImpl) -> PResult<AstImpl> {
        i.impl_ty = i.impl_ty.visit(&mut DenyAssociatedTypes)?;
        i.trait_ty = i.trait_ty.visit(&mut DenyAssociatedTypes)?;

        Ok(i)
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.inside_method = false;

        Ok(f)
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.inside_method = false;

        Ok(o)
    }
}

struct DenyAssociatedTypes;

impl AstAdapter for DenyAssociatedTypes {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::AssociatedType { .. } = &t {
            return perror!("Unexpected associated type `{}`", t);
        }

        Ok(t)
    }
}
