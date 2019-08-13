use crate::analyze::represent::*;
use crate::parser::*;
use crate::util::result::{Expect, PError, PResult};
use crate::util::{Counter, Span, StackMap};
use std::collections::HashMap;

pub struct GenericsAdapter {
    pub functions: HashMap<String, AnFunctionData>,
    pub traits: HashMap<String, AnTraitData>,
    pub objects: HashMap<String, AnObjectData>,
    pub impls: Vec<AnImplData>,

    method_to_trait: HashMap<String, String>,
    current_trait: Option<String>,

    generic_scope: StackMap<String, usize>,
    generic_counter: Counter,
}

impl GenericsAdapter {
    pub fn new() -> GenericsAdapter {
        GenericsAdapter {
            functions: HashMap::new(),
            traits: HashMap::new(),
            objects: HashMap::new(),
            impls: Vec::new(),
            method_to_trait: HashMap::new(),
            current_trait: None,
            generic_scope: StackMap::new(),
            generic_counter: Counter::new(0),
        }
    }

    fn expect_generics(
        span: Span,
        generics: Vec<AstType>,
        expected: usize,
    ) -> PResult<Vec<AstType>> {
        let got = generics.len();

        if got == expected {
            Ok(generics.clone())
        } else if got == 0 {
            Ok((0..expected).map(|_| AstType::Infer).collect())
        } else {
            PError::new(
                span,
                format!("Mismatched generics: Expected {}, got {}.", expected, got),
            )
        }
    }

    fn register_generics(&mut self, span: Span, generics: &Vec<String>) -> PResult<Vec<usize>> {
        let mut ids = Vec::new();

        for generic in generics {
            let id = self.generic_counter.next();

            self.generic_scope
                .get(generic)
                .not_expected(span, "generic", generic)?;
            self.generic_scope.add(generic.clone(), id);

            ids.push(id);
        }

        Ok(ids)
    }

    fn process_type(&mut self, ty: &AstType) -> PResult<AstType> {
        // This is a bit... uh.... roundabout. TODO: fix this.
        ty.clone().visit(self)
    }

    fn process_restrictions(
        &mut self,
        restrictions: &Vec<AstTypeRestriction>,
    ) -> PResult<Vec<AstTypeRestriction>> {
        restrictions.clone().visit(self)
    }

    fn process_parameters(&mut self, params: &Vec<AstNamedVariable>) -> PResult<Vec<AstType>> {
        let params: Vec<_> = params.iter().map(|p| p.ty.clone()).collect();
        params.visit(self)
    }

    fn process_members(
        &mut self,
        members: &Vec<AstObjectMember>,
    ) -> PResult<HashMap<String, AstType>> {
        let mut member_assoc = HashMap::new();

        for AstObjectMember {
            span,
            name,
            member_type,
        } in members
        {
            let member_type = member_type.clone().visit(self)?;
            member_assoc
                .insert(name.clone(), member_type)
                .not_expected(*span, "member", name)?;
        }

        Ok(member_assoc)
    }

    pub fn second_pass(&mut self) -> GenericsAdapterSecondPass {
        GenericsAdapterSecondPass(self)
    }
}

impl Adapter for GenericsAdapter {
    fn enter_fn_signature(&mut self, f: AstFnSignature) -> PResult<AstFnSignature> {
        self.generic_scope.reset();

        let fn_data = AnFunctionData {
            generics: self.register_generics(f.name_span, &f.generics)?,
            parameters: self.process_parameters(&f.parameter_list)?,
            return_type: self.process_type(&f.return_type)?,
            restrictions: self.process_restrictions(&f.restrictions)?,
        };

        self.functions
            .insert(f.name.clone(), fn_data)
            .not_expected(f.name_span, "function", &f.name)?;

        Ok(f)
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.generic_scope.reset();

        let obj_data = AnObjectData {
            generics: self.register_generics(o.name_span, &o.generics)?,
            members: self.process_members(&o.members)?,
            restrictions: self.process_restrictions(&o.restrictions)?,
        };

        self.objects.insert(o.name.clone(), obj_data).not_expected(
            o.name_span,
            "object",
            &o.name,
        )?;

        Ok(o)
    }

    fn enter_object_fn_signature(
        &mut self,
        o: AstObjectFnSignature,
    ) -> PResult<AstObjectFnSignature> {
        self.generic_scope.reset();

        let has_self = o.has_self;
        let fn_data = AnFunctionData {
            generics: self.register_generics(o.name_span, &o.generics)?,
            parameters: self.process_parameters(&o.parameter_list)?,
            return_type: self.process_type(&o.return_type)?,
            restrictions: self.process_restrictions(&o.restrictions)?,
        };

        // TODO: ew.
        if let Some(ref trait_name) = self.current_trait {
            let an_trait = self.traits.get_mut(trait_name).unwrap(); /* Really should NEVER fail...! */

            if has_self {
                self.method_to_trait
                    .insert(o.name.clone(), trait_name.clone());
            }

            an_trait
                .methods
                .insert(o.name.clone(), fn_data)
                .not_expected(o.name_span, "method", &o.name)?;
        }

        Ok(o)
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.generic_scope.reset();

        self.current_trait = Some(t.name.clone());

        let trait_data = AnTraitData {
            generics: self.register_generics(t.name_span, &t.generics)?,
            methods: HashMap::new(),
            restrictions: self.process_restrictions(&t.restrictions)?,
        };

        self.traits
            .insert(t.name.clone(), trait_data)
            .not_expected(t.name_span, "trait", &t.name)?;

        Ok(t)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.generic_scope.reset();

        let impl_data = AnImplData {
            generics: self.register_generics(i.name_span, &i.generics)?,
            trait_ty: self.process_type(&i.trait_ty)?,
            impl_ty: self.process_type(&i.impl_ty)?,
            restrictions: self.process_restrictions(&i.restrictions)?,
        };

        self.impls.push(impl_data);

        // Just check that we're impl'ing a trait after all...
        // We cannot `impl i32 for i32`...
        if let AstType::Object(ref name, _) = i.trait_ty {
            if !self.traits.contains_key(name) {
                PError::new(
                    i.name_span,
                    format!("The impl provided is not implementing a trait type."),
                )?;
            }
        } else {
            PError::new(
                i.name_span,
                format!("The impl provided is not implementing a trait type."),
            )?;
        }

        Ok(i)
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Generic(name) => {
                if let Some(id) = self.generic_scope.get(&name) {
                    Ok(AstType::GenericPlaceholder(id, name))
                } else {
                    PError::new(Span::new(0, 0), format!("Unknown generic `{}`", name))?
                }
            }
            t => Ok(t),
        }
    }

    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.current_trait = None;

        Ok(t)
    }
}

pub struct GenericsAdapterSecondPass<'a>(&'a mut GenericsAdapter);

impl<'a> Adapter for GenericsAdapterSecondPass<'a> {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Object(name, generics) => {
                let expected = if self.0.traits.contains_key(&name) {
                    self.0.traits[&name].generics.len()
                } else if self.0.objects.contains_key(&name) {
                    self.0.objects[&name].generics.len()
                } else {
                    PError::new(Span::new(0, 0), format!("Unknown type `{}`", name))?
                };

                Ok(AstType::Object(
                    name,
                    GenericsAdapter::expect_generics(Span::new(0, 0), generics, expected)?,
                ))
            }
            t => Ok(t),
        }
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Call {
                name,
                generics,
                args,
            } => {
                let fun = self
                    .0
                    .functions
                    .get(&name)
                    .expected(e.span, "function", &name)?;
                let expected = fun.generics.len(); // TODO: this is ugly.
                let generics = GenericsAdapter::expect_generics(e.span, generics, expected)?;

                AstExpressionData::Call {
                    name,
                    generics,
                    args,
                }
            }
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                mut args,
            } => {
                // Insert 'self.0' parameter.
                args.insert(0, *object);

                // Recursively visits static call
                return AstExpression::static_call(span, AstType::Infer, fn_name, generics, args)
                    .visit(self.0);
            }
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
            } => {
                let obj_name = self
                    .0
                    .method_to_trait
                    .get(&fn_name)
                    .expected(e.span, "trait method", &fn_name)?
                    .clone();

                if let Some(ref expected_obj_name) = associated_trait {
                    if *expected_obj_name != obj_name {
                        //TODO: Die.
                    }
                }

                if let AstType::Object(ref obj_name, ..) = call_type {
                    if self.0.traits.contains_key(obj_name) {
                        // TODO: die.
                    }
                }

                let trt = self
                    .0
                    .traits
                    .get(&obj_name)
                    .expected(e.span, "trait", &obj_name)?;
                let fun = trt
                    .methods
                    .get(&fn_name)
                    .expected(e.span, "method", &fn_name)?;
                let expected_fn = fun.generics.len();
                let fn_generics =
                    GenericsAdapter::expect_generics(e.span, fn_generics, expected_fn)?;

                AstExpressionData::StaticCall {
                    call_type,
                    fn_name,
                    fn_generics,
                    args,
                    associated_trait: Some(obj_name),
                }
            }
            t => t,
        };

        Ok(AstExpression { data, ty, span })
    }
}
