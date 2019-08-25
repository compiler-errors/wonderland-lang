use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::util::result::*;
use crate::util::{Span, StackMap};
use std::collections::{HashMap, HashSet};

pub struct GenericsAdapter<'a> {
    function_generic_count: &'a mut HashMap<String, usize>,
    trait_generic_count: &'a mut HashMap<String, usize>,
    trait_method_generic_count: &'a mut HashMap<(String, String), usize>,
    object_generic_count: &'a mut HashMap<String, usize>,

    method_to_trait: &'a mut HashMap<String, String>,
    type_to_trait: &'a mut HashMap<String, String>,

    current_trait: Option<String>,

    generic_scope: StackMap<String, AstGeneric>,
}

impl<'a> GenericsAdapter<'a> {
    pub fn new(
        function_generic_count: &'a mut HashMap<String, usize>,
        trait_generic_count: &'a mut HashMap<String, usize>,
        trait_method_generic_count: &'a mut HashMap<(String, String), usize>,
        object_generic_count: &'a mut HashMap<String, usize>,

        method_to_trait: &'a mut HashMap<String, String>,
        type_to_trait: &'a mut HashMap<String, String>,
    ) -> GenericsAdapter<'a> {
        GenericsAdapter {
            function_generic_count,
            trait_generic_count,
            trait_method_generic_count,
            object_generic_count,
            method_to_trait,
            type_to_trait,
            current_trait: None,
            generic_scope: StackMap::new(),
        }
    }

    fn register_generics(&mut self, span: Span, generics: &Vec<AstGeneric>) -> PResult<()> {
        for g in generics {
            self.generic_scope
                .get_top(&g.1)
                .not_expected(span, "generic", &g.1)?;
            self.generic_scope.add(g.1.clone(), g.clone());
        }

        Ok(())
    }
}

impl<'a> Adapter for GenericsAdapter<'a> {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.generic_scope.reset();
        self.register_generics(f.name_span, &f.generics)?;

        self.function_generic_count
            .insert(f.name.clone(), f.generics.len())
            .not_expected(f.name_span, "function", &f.name)?;

        Ok(f)
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Generic(name) => {
                let id =
                    self.generic_scope
                        .get(&name)
                        .expected(Span::new(0, 0), "generic", &name)?;

                Ok(id.clone().into())
            }
            t => Ok(t),
        }
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.generic_scope.reset();
        self.register_generics(o.name_span, &o.generics)?;

        self.object_generic_count
            .insert(o.name.clone(), o.generics.len())
            .not_expected(o.name_span, "object", &o.name)?;

        Ok(o)
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.generic_scope.push();
        self.register_generics(o.name_span, &o.generics)?;

        // TODO: ew.
        if let Some(ref trait_name) = self.current_trait {
            // TODO: Maybe when I have elaborated types, then I can make this only a thing for has_self methods.
            self.method_to_trait
                .insert(o.name.clone(), trait_name.clone());

            self.trait_method_generic_count
                .insert((trait_name.clone(), o.name.clone()), o.generics.len());
        }

        Ok(o)
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.generic_scope.reset();
        self.register_generics(t.name_span, &t.generics)?;
        self.current_trait = Some(t.name.clone());

        for associated_ty in t.associated_types.values() {
            if &associated_ty.name == "Self" {
                continue;
            }

            self.type_to_trait
                .insert(associated_ty.name.clone(), t.name.clone())
                .not_expected(Span::new(0, 0), "associated type", &associated_ty.name)?;
        }

        self.trait_generic_count
            .insert(t.name.clone(), t.generics.len())
            .not_expected(t.name_span, "trait", &t.name)?;

        Ok(t)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.generic_scope.reset();
        self.register_generics(i.name_span, &i.generics)?;

        // TODO: Move this to another adapter...
        // Quick and dirty check for impl fn name uniqueness.
        let mut fn_names = HashSet::new();
        for fun in &i.fns {
            if !fn_names.insert(fun.name.clone()) {
                return PError::new(
                    fun.name_span,
                    format!("Duplicated function in impl: `{}`", fun.name),
                );
            }
        }

        Ok(i)
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.generic_scope.pop();
        Ok(o)
    }

    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.current_trait = None;

        Ok(t)
    }
}
