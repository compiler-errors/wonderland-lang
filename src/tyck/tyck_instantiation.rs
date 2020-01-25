use crate::{
    ana::represent::AnalyzedProgram,
    parser::{
        ast::{
            AstGeneric, AstTraitType, AstTraitTypeWithAssocs, AstType, AstTypeRestriction,
            GenericId, ImplId, ModuleRef,
        },
        ast_visitor::AstAdapter,
    },
    util::{PResult, Visit},
};
use std::collections::{BTreeMap, HashMap};

pub fn instantiate_object_restrictions(
    program: &AnalyzedProgram,
    name: &ModuleRef,
    generics: &[AstType],
) -> PResult<Vec<AstTypeRestriction>> {
    let obj_info = &program.analyzed_objects[name];
    let mut generics_adapter = GenericsAdapter::new(&obj_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(AstType::object(name.clone(), generics.to_vec()));
    obj_info
        .restrictions
        .clone()
        .visit(&mut generics_adapter)?
        .visit(&mut self_adapter)
}

pub fn instantiate_enum_restrictions(
    program: &AnalyzedProgram,
    name: &ModuleRef,
    generics: &[AstType],
) -> PResult<Vec<AstTypeRestriction>> {
    let enum_info = &program.analyzed_enums[name];
    let mut generics_adapter = GenericsAdapter::new(&enum_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(AstType::enumerable(name.clone(), generics.to_vec()));
    enum_info
        .restrictions
        .clone()
        .visit(&mut generics_adapter)?
        .visit(&mut self_adapter)
}

pub fn instantiate_fn_signature(
    program: &AnalyzedProgram,
    name: &ModuleRef,
    generics: &[AstType],
) -> PResult<(Vec<AstType>, AstType, Vec<AstTypeRestriction>)> {
    let fn_info = &program.analyzed_functions[name];
    let mut generics_adapter = GenericsAdapter::new(&fn_info.generics, generics);

    Ok((
        fn_info.parameters.clone().visit(&mut generics_adapter)?,
        fn_info.return_type.clone().visit(&mut generics_adapter)?,
        fn_info.restrictions.clone().visit(&mut generics_adapter)?,
    ))
}

pub fn instantiate_trait_fn_signature(
    program: &AnalyzedProgram,
    trait_name: &ModuleRef,
    trait_generics: &[AstType],
    fn_name: &str,
    fn_generics: &[AstType],
    self_ty: &AstType,
) -> PResult<(Vec<AstType>, AstType, Vec<AstTypeRestriction>)> {
    let trt_info = &program.analyzed_traits[trait_name];
    let fn_info = &trt_info.methods[fn_name];

    let mut trt_generics_adapter = GenericsAdapter::new(&trt_info.generics, trait_generics);
    let mut fn_generics_adapter = GenericsAdapter::new(&fn_info.generics, fn_generics);
    let mut self_adapter = SelfAdapter::new(self_ty.clone());

    Ok((
        fn_info
            .parameters
            .clone()
            .visit(&mut trt_generics_adapter)?
            .visit(&mut fn_generics_adapter)?
            .visit(&mut self_adapter)?,
        fn_info
            .return_type
            .clone()
            .visit(&mut trt_generics_adapter)?
            .visit(&mut fn_generics_adapter)?
            .visit(&mut self_adapter)?,
        fn_info
            .restrictions
            .clone()
            .visit(&mut trt_generics_adapter)?
            .visit(&mut fn_generics_adapter)?
            .visit(&mut self_adapter)?,
    ))
}

pub fn instantiate_object_members(
    program: &AnalyzedProgram,
    name: &ModuleRef,
    generics: &[AstType],
) -> PResult<HashMap<String, AstType>> {
    let obj_info = &program.analyzed_objects[name];
    let mut generics_adapter = GenericsAdapter::new(&obj_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(AstType::object(name.clone(), generics.to_vec()));
    obj_info
        .member_tys
        .clone()
        .visit(&mut generics_adapter)?
        .visit(&mut self_adapter)
}

pub fn instantiate_object_member(
    program: &AnalyzedProgram,
    name: &ModuleRef,
    generics: &[AstType],
    member: &str,
) -> PResult<AstType> {
    let obj_info = &program.analyzed_objects[name];
    let mut generics_adapter = GenericsAdapter::new(&obj_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(AstType::object(name.clone(), generics.to_vec()));
    obj_info.member_tys[member]
        .clone()
        .visit(&mut generics_adapter)?
        .visit(&mut self_adapter)
}

pub fn instantiate_associated_ty(
    program: &AnalyzedProgram,
    imp: ImplId,
    generics: &[AstType],
    name: &str,
    self_ty: &AstType,
) -> PResult<AstType> {
    let impl_info = &program.analyzed_impls[&imp];

    let mut generics_adapter = GenericsAdapter::new(&impl_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(self_ty.clone());

    let t = impl_info.associated_tys[name]
        .clone()
        .visit(&mut generics_adapter)?
        .visit(&mut self_adapter)?;

    debug!(
        "Instantiated <{} as {:?}>::{} = {}",
        impl_info
            .impl_ty
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut self_adapter)?,
        impl_info
            .trait_ty
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut self_adapter)?,
        name,
        t
    );

    debug!(
        "Originally <{} as {:?}>::{} = {}",
        impl_info.impl_ty, impl_info.trait_ty, name, impl_info.associated_tys[name]
    );

    Ok(t)
}

pub fn instantiate_associated_ty_restrictions(
    program: &AnalyzedProgram,
    trt: &AstTraitType,
    name: &str,
    self_ty: &AstType,
) -> PResult<Vec<AstTraitTypeWithAssocs>> {
    let trt_info = &program.analyzed_traits[&trt.name];

    let mut trt_generics_adapter = GenericsAdapter::new(&trt_info.generics, &trt.generics);
    let mut self_adapter = SelfAdapter::new(self_ty.clone());

    trt_info.associated_tys[name]
        .restrictions
        .clone()
        .visit(&mut trt_generics_adapter)?
        .visit(&mut self_adapter)
}

pub fn instantiate_trait_restrictions(
    program: &AnalyzedProgram,
    trt: &AstTraitType,
    self_ty: &AstType,
) -> PResult<Vec<AstTypeRestriction>> {
    let trt_info = &program.analyzed_traits[&trt.name];

    let mut trt_generics_adapter = GenericsAdapter::new(&trt_info.generics, &trt.generics);
    let mut self_adapter = SelfAdapter::new(self_ty.clone());

    trt_info
        .restrictions
        .clone()
        .visit(&mut trt_generics_adapter)?
        .visit(&mut self_adapter)
}

pub fn instantiate_enum_pattern(
    program: &AnalyzedProgram,
    name: &ModuleRef,
    generics: &[AstType],
    variant: &str,
) -> PResult<Vec<AstType>> {
    let enum_info = &program.analyzed_enums[name];
    let mut generics_adapter = GenericsAdapter::new(&enum_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(AstType::enumerable(name.clone(), generics.to_vec()));

    enum_info.variants[variant]
        .fields
        .clone()
        .visit(&mut generics_adapter)?
        .visit(&mut self_adapter)
}

pub fn instantiate_impl_trait_ty(
    program: &AnalyzedProgram,
    imp: ImplId,
    generics: &[AstType],
    self_ty: &AstType,
) -> PResult<Option<AstTraitTypeWithAssocs>> {
    let impl_info = &program.analyzed_impls[&imp];

    let mut generics_adapter = GenericsAdapter::new(&impl_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(self_ty.clone());

    if let Some(trt) = impl_info
        .trait_ty
        .clone()
        .visit(&mut generics_adapter)?
        .visit(&mut self_adapter)?
    {
        Ok(Some(AstTraitTypeWithAssocs {
            trt,
            assoc_bindings: BTreeMap::new(),
        }))
    } else {
        Ok(None)
    }
}

pub fn instantiate_impl_signature(
    program: &AnalyzedProgram,
    imp: ImplId,
    generics: &[AstType],
    self_ty: &AstType,
) -> PResult<(AstType, Option<AstTraitType>, Vec<AstTypeRestriction>)> {
    let impl_info = &program.analyzed_impls[&imp];

    let mut generics_adapter = GenericsAdapter::new(&impl_info.generics, generics);
    let mut self_adapter = SelfAdapter::new(self_ty.clone());

    Ok((
        impl_info
            .impl_ty
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut self_adapter)?,
        impl_info
            .trait_ty
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut self_adapter)?,
        impl_info
            .restrictions
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut self_adapter)?,
    ))
}

pub fn instantiate_impl_fn_signature(
    program: &AnalyzedProgram,
    imp: ImplId,
    generics: &[AstType],
    fn_name: &str,
    fn_generics: &[AstType],
    self_ty: &AstType,
) -> PResult<(Vec<AstType>, AstType, Vec<AstTypeRestriction>)> {
    let impl_info = &program.analyzed_impls[&imp];
    let method_info = &impl_info.methods[fn_name];

    let mut generics_adapter = GenericsAdapter::new(&impl_info.generics, generics);
    let mut fn_generics_adapter = GenericsAdapter::new(&method_info.generics, fn_generics);
    let mut self_adapter = SelfAdapter::new(self_ty.clone());

    Ok((
        method_info
            .parameters
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut fn_generics_adapter)?
            .visit(&mut self_adapter)?,
        method_info
            .return_type
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut fn_generics_adapter)?
            .visit(&mut self_adapter)?,
        method_info
            .restrictions
            .clone()
            .visit(&mut generics_adapter)?
            .visit(&mut fn_generics_adapter)?
            .visit(&mut self_adapter)?,
    ))
}

pub struct GenericsAdapter(HashMap<GenericId, AstType>);

impl GenericsAdapter {
    pub fn new(generics: &[AstGeneric], tys: &[AstType]) -> GenericsAdapter {
        assert_eq!(generics.len(), tys.len());

        GenericsAdapter(
            Iterator::zip(generics.iter(), tys.iter())
                .map(|(g, t)| (g.0, t.clone()))
                .collect(),
        )
    }
}

impl AstAdapter for GenericsAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::GenericPlaceholder(id, _) if self.0.contains_key(&id) =>
                self.0[&id].clone().visit(self),
            t => Ok(t),
        }
    }
}

pub struct SelfAdapter(AstType);

impl SelfAdapter {
    fn new(t: AstType) -> SelfAdapter {
        SelfAdapter(t)
    }
}

impl AstAdapter for SelfAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::SelfType => Ok(self.0.clone()),
            t => Ok(t),
        }
    }
}
