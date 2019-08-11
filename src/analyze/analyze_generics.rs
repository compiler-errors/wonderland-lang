use crate::analyze::represent::*;
use crate::parser::*;
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
    fn expect_generics(generics: Vec<AstType>, expected: usize) -> Vec<AstType> {
        if generics.len() == expected {
            generics.clone()
        } else if generics.len() == 0 {
            (0..expected).map(|_| AstType::Infer).collect()
        } else {
            panic!("mismatched")
        }
    }

    fn register_generics(&mut self, generics: &Vec<String>) -> Vec<usize> {
        generics
            .iter()
            .map(|generic| {
                let id = self.generic_counter.next();

                if self.generic_scope.get(generic).is_some() {
                    panic!("TODO: duplicated generic.")
                }

                self.generic_scope.add(generic.clone(), id);
                id
            })
            .collect()
    }

    fn process_type(&mut self, ty: &AstType) -> AstType {
        // This is a bit... uh.... roundabout. TODO: fix this.
        ty.clone().visit(self)
    }

    fn process_restrictions(
        &mut self,
        restrictions: &Vec<AstTypeRestriction>,
    ) -> Vec<AstTypeRestriction> {
        restrictions.clone().visit(self)
    }

    fn process_parameters(
        &mut self,
        params: &Vec<AstNamedVariable>,
        has_self: bool,
    ) -> Vec<AstType> {
        let mut params: Vec<_> = params.iter().map(|p| p.ty.clone()).collect();

        if has_self {
            params.insert(0, AstType::SelfType);
        }

        params.visit(self)
    }

    fn process_members(&mut self, members: &Vec<AstObjectMember>) -> HashMap<String, AstType> {
        let mut member_assoc = HashMap::new();

        for AstObjectMember { name, member_type } in members {
            if member_assoc.contains_key(name) {
                panic!("Duplicated object member");
            }

            member_assoc.insert(name.clone(), member_type.clone().visit(self));
        }

        member_assoc
    }
}

impl Adapter for GenericsAdapter {
    fn enter_function(&mut self, f: AstFunction) -> AstFunction {
        self.generic_scope.push();

        let fn_data = AnFunctionData {
            generics: self.register_generics(&f.signature.generics),
            parameters: self.process_parameters(&f.signature.parameter_list, false),
            return_type: f.signature.return_type.clone(),
            restrictions: self.process_restrictions(&f.signature.restrictions),
        };

        self.functions.insert(f.signature.name.clone(), fn_data);

        f
    }

    fn enter_object(&mut self, o: AstObject) -> AstObject {
        self.generic_scope.push();

        let obj_data = AnObjectData {
            generics: self.register_generics(&o.generics),
            members: self.process_members(&o.members),
            restrictions: self.process_restrictions(&o.restrictions),
        };

        self.objects.insert(o.name.clone(), obj_data);

        o
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> AstObjectFunction {
        self.generic_scope.push();
        let has_self = o.signature.has_self;
        let fn_data = AnFunctionData {
            generics: self.register_generics(&o.signature.generics),
            parameters: self.process_parameters(&o.signature.parameter_list, has_self),
            return_type: self.process_type(&o.signature.return_type),
            restrictions: self.process_restrictions(&o.signature.restrictions),
        };

        // TODO: ew.
        let mut an_trait = self
            .traits
            .get_mut(self.current_trait.as_ref().unwrap())
            .unwrap();

        an_trait.methods.insert(o.signature.name.clone(), fn_data);

        o
    }

    fn enter_trait(&mut self, t: AstTrait) -> AstTrait {
        self.generic_scope.push();
        self.current_trait = Some(t.name.clone());

        let trait_data = AnTraitData {
            generics: self.register_generics(&t.generics),
            methods: HashMap::new(),
            restrictions: self.process_restrictions(&t.restrictions),
        };

        self.traits.insert(t.name.clone(), trait_data);

        t
    }

    fn enter_impl(&mut self, i: AstImpl) -> AstImpl {
        self.generic_scope.push();

        let impl_data = AnImplData {
            generics: self.register_generics(&i.generics),
            trait_ty: self.process_type(&i.trait_ty),
            impl_ty: self.process_type(&i.impl_ty),
            restrictions: self.process_restrictions(&i.restrictions),
        };

        self.impls.push(impl_data);

        // Just check that we're impl'ing a trait after all...
        // We cannot `impl i32 for i32`...
        if let AstType::Object(ref name, _) = i.trait_ty {
            if !self.traits.contains_key(name) {
                panic!("TODO: Not impl'ing a trait");
            }
        } else {
            panic!("TODO: Not impl'ing a trait");
        }

        i
    }

    fn enter_type(&mut self, t: AstType) -> AstType {
        match t {
            AstType::Object(name, generics) => {
                let expected = if self.traits.contains_key(&name) {
                    self.traits[&name].generics.len()
                } else if self.objects.contains_key(&name) {
                    self.objects[&name].generics.len()
                } else {
                    panic!("TODO: Unknown type");
                };

                AstType::Object(name, Self::expect_generics(generics, expected))
            }
            AstType::Generic(name) => {
                if let Some(id) = self.generic_scope.get(&name) {
                    AstType::GenericPlaceholder(id, name)
                } else {
                    panic!("TODO: Oops.");
                }
            }
            t => t,
        }
    }

    fn enter_expression(&mut self, mut e: AstExpression) -> AstExpression {
        match e {
            AstExpression::Call {
                name,
                generics,
                args,
            } => {
                let expected = self.functions.get(&name).unwrap().generics.len(); // TODO: this is ugly.
                let generics = Self::expect_generics(generics, expected);

                AstExpression::Call {
                    name,
                    generics,
                    args,
                }
            }
            AstExpression::ObjectCall {
                object,
                fn_name,
                generics,
                mut args,
            } => {
                let object_name = self.method_to_trait.get(&fn_name).unwrap().clone();

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
                let trt = self.traits.get(&obj_name).unwrap();
                let expected_obj = trt.generics.len();
                let obj_generics = Self::expect_generics(obj_generics, expected_obj);

                let fun = trt.methods.get(&fn_name).unwrap();
                let expected_fn = fun.generics.len();
                let fn_generics = Self::expect_generics(fn_generics, expected_obj);

                if args.len() != fun.parameters.len() {
                    panic!("TODO: Mismatched arguments!");
                }

                AstExpression::StaticCall {
                    obj_name,
                    obj_generics,
                    fn_name,
                    fn_generics,
                    args,
                }
            }
            t => t,
        }
    }
}
