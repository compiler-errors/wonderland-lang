use crate::analyze::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::util::result::*;
use crate::util::{Counter, Span, StackMap};
use std::collections::HashMap;

pub struct GenericsAdapter {
    pub functions: HashMap<String, AnFunctionData>,
    pub traits: HashMap<String, AnTraitData>,
    pub objects: HashMap<String, AnObjectData>,
    pub impls: HashMap<ImplId, AnImplData>,
    pub method_to_trait: HashMap<String, String>,
    pub type_to_trait: HashMap<String, String>,

    current_trait: Option<String>,

    generic_scope: StackMap<String, GenericId>,
    generic_counter: Counter,
}

impl GenericsAdapter {
    pub fn new() -> GenericsAdapter {
        GenericsAdapter {
            functions: HashMap::new(),
            traits: HashMap::new(),
            objects: HashMap::new(),
            impls: HashMap::new(),
            method_to_trait: HashMap::new(),
            type_to_trait: HashMap::new(),
            current_trait: None,
            generic_scope: StackMap::new(),
            generic_counter: Counter::new(0),
        }
    }

    fn register_generics(&mut self, span: Span, generics: &Vec<String>) -> PResult<Vec<GenericId>> {
        let mut ids = Vec::new();

        for generic in generics {
            let id = GenericId(self.generic_counter.next());

            self.generic_scope
                .get(generic)
                .not_expected(span, "generic", generic)?;
            self.generic_scope.add(generic.clone(), id);

            ids.push(id);
        }

        Ok(ids)
    }

    fn clone_visit_type(&mut self, ty: &AstType) -> PResult<AstType> {
        // This is a bit... uh.... roundabout. TODO: fix this.
        ty.clone().visit(self)
    }

    fn clone_visit_trait_type(&mut self, ty: &AstTraitType) -> PResult<AstTraitType> {
        Ok(AstTraitType(ty.0.clone(), ty.1.clone().visit(self)?))
    }

    fn clone_visit_restrictions(
        &mut self,
        restrictions: &Vec<AstTypeRestriction>,
    ) -> PResult<Vec<AstTypeRestriction>> {
        restrictions.clone().visit(self)
    }

    fn clone_visit_parameters(&mut self, params: &Vec<AstNamedVariable>) -> PResult<Vec<AstType>> {
        let params: Vec<_> = params.iter().map(|p| p.ty.clone()).collect();
        params.visit(self)
    }

    fn clone_visit_members(
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

    fn clone_trait_associated_tys(
        &mut self,
        associated_tys: &HashMap<String, AstAssociatedType>,
    ) -> PResult<HashMap<String, AstAssociatedType>> {
        let mut ty_assoc = HashMap::new();

        for (k, v) in associated_tys {
            let v = v.clone().visit(self)?;
            ty_assoc
                .insert(k.clone(), v)
                .not_expected(Span::new(0, 0), "associated type", k)?;
        }

        Ok(ty_assoc)
    }

    fn clone_impl_associated_tys(
        &mut self,
        associated_tys: &HashMap<String, AstType>,
    ) -> PResult<HashMap<String, AstType>> {
        let mut ty_assoc = HashMap::new();

        for (k, v) in associated_tys {
            let v = v.clone().visit(self)?;
            ty_assoc
                .insert(k.clone(), v)
                .not_expected(Span::new(0, 0), "associated type", k)?;
        }

        Ok(ty_assoc)
    }
}

impl Adapter for GenericsAdapter {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.generic_scope.reset();

        let fn_data = AnFunctionData {
            generics: self.register_generics(f.name_span, &f.generics)?,
            parameters: self.clone_visit_parameters(&f.parameter_list)?,
            return_type: self.clone_visit_type(&f.return_type)?,
            restrictions: self.clone_visit_restrictions(&f.restrictions)?,
        };

        self.functions
            .insert(f.name.clone(), fn_data)
            .not_expected(f.name_span, "function", &f.name)?;

        Ok(f)
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

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.generic_scope.reset();

        let obj_data = AnObjectData {
            generics: self.register_generics(o.name_span, &o.generics)?,
            members: self.clone_visit_members(&o.members)?,
            restrictions: self.clone_visit_restrictions(&o.restrictions)?,
        };

        self.objects.insert(o.name.clone(), obj_data).not_expected(
            o.name_span,
            "object",
            &o.name,
        )?;

        Ok(o)
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.generic_scope.push();
        let fn_data = AnFunctionData {
            generics: self.register_generics(o.name_span, &o.generics)?,
            parameters: self.clone_visit_parameters(&o.parameter_list)?,
            return_type: self.clone_visit_type(&o.return_type)?,
            restrictions: self.clone_visit_restrictions(&o.restrictions)?,
        };

        // TODO: ew.
        if let Some(ref trait_name) = self.current_trait {
            /* This unwrap really should NEVER fail...! */
            let an_trait = self.traits.get_mut(trait_name).unwrap();

            if o.has_self {
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
            restrictions: self.clone_visit_restrictions(&t.restrictions)?,
            associated_tys: self.clone_trait_associated_tys(&t.associated_types)?,
            impls: Vec::new(),
        };

        for associated_ty in t.associated_types.values() {
            self.type_to_trait
                .insert(associated_ty.name.clone(), t.name.clone())
                .not_expected(Span::new(0, 0), "associated type", &associated_ty.name)?;
        }

        self.traits
            .insert(t.name.clone(), trait_data)
            .not_expected(t.name_span, "trait", &t.name)?;

        Ok(t)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.generic_scope.reset();

        let impl_data = AnImplData {
            impl_id: i.impl_id,
            generics: self.register_generics(i.name_span, &i.generics)?,
            trait_ty: self.clone_visit_trait_type(&i.trait_ty)?,
            impl_ty: self.clone_visit_type(&i.impl_ty)?,
            restrictions: self.clone_visit_restrictions(&i.restrictions)?,
            associated_tys: self.clone_impl_associated_tys(&i.associated_types)?,
        };

        self.impls.insert(i.impl_id, impl_data);

        let trait_name = &i.trait_ty.0;
        let trt = self
            .traits
            .get_mut(trait_name)
            .expected(i.name_span, "trait", trait_name)?;

        trt.impls.push(i.impl_id);

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
