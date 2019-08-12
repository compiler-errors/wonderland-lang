use crate::analyze::represent::*;
use crate::parser::*;
use crate::util::result::{Expect, PError, PResult};
use crate::util::{Counter, StackMap};
use std::collections::HashMap;

struct GenericsAdapter {
    functions: HashMap<String, AnFunctionData>,
    traits: HashMap<String, AnTraitData>,
    objects: HashMap<String, AnObjectData>,
    impls: Vec<AnImplData>,

    method_to_trait: HashMap<String, String>,
    current_trait: Option<String>,

    generic_scope: StackMap<String, usize>,
    generic_counter: Counter,
}

impl GenericsAdapter {
    fn expect_generics(generics: Vec<AstType>, expected: usize) -> PResult<Vec<AstType>> {
        let got = generics.len();

        if got == expected {
            Ok(generics.clone())
        } else if got == 0 {
            Ok((0..expected).map(|_| AstType::Infer).collect())
        } else {
            PError::new(
                0,
                format!("Mismatched generics: Expected {}, got {}.", expected, got),
            )
        }
    }

    fn register_generics(&mut self, generics: &Vec<String>) -> PResult<Vec<usize>> {
        let mut ids = Vec::new();

        for generic in generics {
            let id = self.generic_counter.next();

            self.generic_scope
                .get(generic)
                .not_expected("generic", generic)?;
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

    fn process_parameters(
        &mut self,
        params: &Vec<AstNamedVariable>,
        has_self: bool,
    ) -> PResult<Vec<AstType>> {
        let mut params: Vec<_> = params.iter().map(|p| p.ty.clone()).collect();

        if has_self {
            params.insert(0, AstType::SelfType);
        }

        params.visit(self)
    }

    fn process_members(
        &mut self,
        members: &Vec<AstObjectMember>,
    ) -> PResult<HashMap<String, AstType>> {
        let mut member_assoc = HashMap::new();

        for AstObjectMember { name, member_type } in members {
            let member_type = member_type.clone().visit(self)?;
            member_assoc
                .insert(name.clone(), member_type)
                .not_expected("member", name)?;
        }

        Ok(member_assoc)
    }
}

impl Adapter for GenericsAdapter {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.generic_scope.push();

        let fn_data = AnFunctionData {
            generics: self.register_generics(&f.signature.generics)?,
            parameters: self.process_parameters(&f.signature.parameter_list, false)?,
            return_type: self.process_type(&f.signature.return_type)?,
            restrictions: self.process_restrictions(&f.signature.restrictions)?,
        };

        self.functions
            .insert(f.signature.name.clone(), fn_data)
            .not_expected("function", &f.signature.name)?;

        Ok(f)
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.generic_scope.push();

        let obj_data = AnObjectData {
            generics: self.register_generics(&o.generics)?,
            members: self.process_members(&o.members)?,
            restrictions: self.process_restrictions(&o.restrictions)?,
        };

        self.objects
            .insert(o.name.clone(), obj_data)
            .not_expected("object", &o.name)?;

        Ok(o)
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.generic_scope.push();
        let has_self = o.signature.has_self;
        let fn_data = AnFunctionData {
            generics: self.register_generics(&o.signature.generics)?,
            parameters: self.process_parameters(&o.signature.parameter_list, has_self)?,
            return_type: self.process_type(&o.signature.return_type)?,
            restrictions: self.process_restrictions(&o.signature.restrictions)?,
        };

        // TODO: ew.
        let trait_name = self
            .current_trait
            .as_ref()
            .expected("trait", "<current trait name>")?;
        let mut an_trait = self
            .traits
            .get_mut(trait_name)
            .expected("trait", trait_name)?;

        an_trait
            .methods
            .insert(o.signature.name.clone(), fn_data)
            .not_expected("method", &o.signature.name)?;

        Ok(o)
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.generic_scope.push();
        self.current_trait = Some(t.name.clone());

        let trait_data = AnTraitData {
            generics: self.register_generics(&t.generics)?,
            methods: HashMap::new(),
            restrictions: self.process_restrictions(&t.restrictions)?,
        };

        self.traits
            .insert(t.name.clone(), trait_data)
            .not_expected("trait", &t.name)?;

        Ok(t)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.generic_scope.push();

        let impl_data = AnImplData {
            generics: self.register_generics(&i.generics)?,
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
                    0,
                    format!("The impl provided is not implementing a trait type."),
                )?;
            }
        } else {
            PError::new(
                0,
                format!("The impl provided is not implementing a trait type."),
            )?;
        }

        Ok(i)
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Object(name, generics) => {
                let expected = if self.traits.contains_key(&name) {
                    self.traits[&name].generics.len()
                } else if self.objects.contains_key(&name) {
                    self.objects[&name].generics.len()
                } else {
                    PError::new(0, format!("Unknown type `{}`", name))?
                };

                Ok(AstType::Object(
                    name,
                    Self::expect_generics(generics, expected)?,
                ))
            }
            AstType::Generic(name) => {
                if let Some(id) = self.generic_scope.get(&name) {
                    Ok(AstType::GenericPlaceholder(id, name))
                } else {
                    PError::new(0, format!("Unknown generic `{}`", name))?
                }
            }
            t => Ok(t),
        }
    }

    fn enter_expression(&mut self, mut e: AstExpression) -> PResult<AstExpression> {
        match e {
            AstExpression::Call {
                name,
                generics,
                args,
            } => {
                let fun = self.functions.get(&name).expected("function", &name)?;
                let expected = fun.generics.len(); // TODO: this is ugly.
                let generics = Self::expect_generics(generics, expected)?;

                Ok(AstExpression::Call {
                    name,
                    generics,
                    args,
                })
            }
            AstExpression::ObjectCall {
                object,
                fn_name,
                generics,
                mut args,
            } => {
                let object_name = self
                    .method_to_trait
                    .get(&fn_name)
                    .expected("trait", &format!("<trait>::{}", fn_name))?
                    .clone();

                // Insert 'self' parameter.
                args.insert(0, *object);

                // Recursively visits static call
                AstExpression::static_call(object_name, Vec::new(), fn_name, generics, args)
                    .visit(self)
            }
            AstExpression::StaticCall {
                obj_name,
                obj_generics,
                fn_name,
                fn_generics,
                args,
            } => {
                let trt = self.traits.get(&obj_name).expected("trait", &obj_name)?;
                let expected_obj = trt.generics.len();
                let obj_generics = Self::expect_generics(obj_generics, expected_obj)?;

                let fun = trt.methods.get(&fn_name).expected("method", &fn_name)?;
                let expected_fn = fun.generics.len();
                let fn_generics = Self::expect_generics(fn_generics, expected_obj)?;

                if args.len() != fun.parameters.len() {
                    PError::new(
                        0,
                        format!(
                            "Mismatched arguments. Expected `{}`, found `{}`.",
                            fun.parameters.len(),
                            args.len()
                        ),
                    )?;
                }

                Ok(AstExpression::StaticCall {
                    obj_name,
                    obj_generics,
                    fn_name,
                    fn_generics,
                    args,
                })
            }
            t => Ok(t),
        }
    }
}
