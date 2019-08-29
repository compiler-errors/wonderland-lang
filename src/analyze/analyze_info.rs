use crate::analyze::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::util::result::*;

use std::collections::{HashMap, HashSet};

pub struct InfoAdapter {
    variable_ids: HashMap<VariableId, AstNamedVariable>,

    functions: HashMap<String, AnFunctionData>,
    traits: HashMap<String, AnTraitData>,
    objects: HashMap<String, AnObjectData>,
    impls: HashMap<ImplId, AnImplData>,

    current_trait: Option<String>,
    current_impl: Option<ImplId>,
}

impl InfoAdapter {
    pub fn new(variable_ids: HashMap<VariableId, AstNamedVariable>) -> InfoAdapter {
        InfoAdapter {
            variable_ids,
            functions: HashMap::new(),
            traits: HashMap::new(),
            objects: HashMap::new(),
            impls: HashMap::new(),
            current_trait: None,
            current_impl: None,
        }
    }

    fn map_generics(&mut self, generics: &Vec<AstGeneric>) -> PResult<Vec<GenericId>> {
        Ok(generics.clone().into_iter().map(|g| g.0).collect())
    }

    fn map_parameters(&mut self, generics: &Vec<AstNamedVariable>) -> PResult<Vec<AstType>> {
        Ok(generics.clone().into_iter().map(|n| n.ty).collect())
    }

    fn clone_members(members: &Vec<AstObjectMember>) -> PResult<HashMap<String, AstType>> {
        let mut member_assoc = HashMap::new();

        for AstObjectMember {
            span,
            name,
            member_type,
        } in members
        {
            member_assoc
                .insert(name.clone(), member_type.clone())
                .not_expected(*span, "member", name)?;
        }

        Ok(member_assoc)
    }
}

impl From<InfoAdapter> for AnalyzedFile {
    fn from(i: InfoAdapter) -> AnalyzedFile {
        AnalyzedFile {
            variable_ids: i.variable_ids,
            analyzed_functions: i.functions,
            analyzed_objects: i.objects,
            analyzed_traits: i.traits,
            analyzed_impls: i.impls,
        }
    }
}

impl Adapter for InfoAdapter {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        let fn_data = AnFunctionData {
            generics: self.map_generics(&f.generics)?,
            parameters: self.map_parameters(&f.parameter_list)?,
            return_type: f.return_type.clone(),
            restrictions: f.restrictions.clone(),
        };

        self.functions
            .insert(f.name.clone(), fn_data)
            .not_expected(f.name_span, "function", &f.name)?;

        Ok(f)
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        let member_indices = o.members.iter().enumerate().map(|(idx, m)| (m.name.clone(), idx)).collect();

        let obj_data = AnObjectData {
            generics: self.map_generics(&o.generics)?,
            member_tys: Self::clone_members(&o.members)?,
            member_indices,
            restrictions: o.restrictions.clone(),
        };

        self.objects.insert(o.name.clone(), obj_data).not_expected(
            o.name_span,
            "object",
            &o.name,
        )?;

        Ok(o)
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        let fn_data = AnFunctionData {
            generics: self.map_generics(&o.generics)?,
            parameters: self.map_parameters(&o.parameter_list)?,
            return_type: o.return_type.clone(),
            restrictions: o.restrictions.clone(),
        };

        // TODO: ew.
        if let Some(ref trait_name) = self.current_trait {
            /* This unwrap really should NEVER fail...! */
            let an_trait = self.traits.get_mut(trait_name).unwrap();

            an_trait
                .methods
                .insert(o.name.clone(), fn_data)
                .not_expected(o.name_span, "method", &o.name)?;
        } else if let Some(ref impl_id) = self.current_impl {
            let an_impl = self.impls.get_mut(impl_id).unwrap();

            an_impl
                .methods
                .insert(o.name.clone(), fn_data)
                .not_expected(o.name_span, "method", &o.name)?;
        }

        Ok(o)
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.current_trait = Some(t.name.clone());

        let trait_data = AnTraitData {
            generics: self.map_generics(&t.generics)?,
            methods: HashMap::new(),
            restrictions: t.restrictions.clone(),
            associated_tys: t.associated_types.clone(),
            impls: Vec::new(),
        };

        self.traits
            .insert(t.name.clone(), trait_data)
            .not_expected(t.name_span, "trait", &t.name)?;

        Ok(t)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.current_impl = Some(i.impl_id);

        let impl_data = AnImplData {
            impl_id: i.impl_id,
            generics: self.map_generics(&i.generics)?,
            methods: HashMap::new(),
            trait_ty: i.trait_ty.clone(),
            impl_ty: i.impl_ty.clone(),
            restrictions: i.restrictions.clone(),
            associated_tys: i.associated_types.clone(),
            is_dummy: false,
        };

        self.impls.insert(i.impl_id, impl_data);

        let trait_name = &i.trait_ty.0;
        let trt = self
            .traits
            .get_mut(trait_name)
            .expected(i.name_span, "trait", trait_name)?;

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

        trt.impls.push(i.impl_id);

        Ok(i)
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        Ok(o)
    }

    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.current_trait = None;

        Ok(t)
    }

    fn exit_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.current_impl = None;

        Ok(i)
    }
}
