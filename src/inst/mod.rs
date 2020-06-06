pub use crate::inst::represent::*;
use crate::{
    ana::{analyze_item_inplace, represent::*},
    ast::{visitor::AstAdapter, *},
    cheshire_quote,
    tyck::*,
    util::{Context, Expect, PResult, Span, Visit},
};
use decorate::decorate_dynamic_fn;
use either::Either;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    rc::Rc,
};
use tyck_instantiation::instantiate_trait_fn_signature;

pub mod decorate;
mod represent;

struct InstantiationAdapter {
    analyzed_program: Rc<AnalyzedProgram>,
    program_for_analysis: AnalyzedProgram,
    base_solver: TyckSolver,

    fns: HashMap<ModuleRef, AstFunction>,
    objects: HashMap<ModuleRef, AstObject>,
    enums: HashMap<ModuleRef, AstEnum>,
    impls: HashMap<ImplId, AstImpl>,
    obj_fns: HashMap<(ImplId, String), AstObjectFunction>,

    instantiated_fns: HashMap<InstFunctionSignature, Option<AstFunction>>,
    instantiated_object_fns: HashMap<InstObjectFunctionSignature, Option<AstObjectFunction>>,
    instantiated_impls: HashMap<InstImplSignature, Option<AstImpl>>,
    instantiated_objects: HashMap<InstObjectSignature, Option<AstObject>>,
    instantiated_enums: HashMap<InstEnumSignature, Option<InstEnumRepresentation>>,
    instantiated_types: HashSet<AstType>,

    solved_impls: HashMap<TyckObjective, InstImplSignature>,
}

#[derive(Debug)]
pub struct InstantiatedProgram {
    pub main_fn: ModuleRef,
    pub instantiated_fns: HashMap<InstFunctionSignature, AstFunction>,
    pub instantiated_object_fns: HashMap<InstObjectFunctionSignature, AstObjectFunction>,
    pub instantiated_impls: HashMap<InstImplSignature, AstImpl>,
    pub instantiated_objects: HashMap<InstObjectSignature, AstObject>,
    pub instantiated_enums: HashMap<InstEnumSignature, InstEnumRepresentation>,
    pub instantiated_globals: HashMap<ModuleRef, AstGlobalVariable>,
    pub instantiated_types: HashSet<AstType>,
}

pub fn instantiate(
    analyzed_program: AnalyzedProgram,
    parsed_program: AstProgram,
) -> PResult<InstantiatedProgram> {
    let mut main_finder = MainFinder(None);

    let mut dynamic_assumptions = TyckDynamicAssumptionAdapter::new(analyzed_program);
    let parsed_program = parsed_program
        .visit(&mut main_finder)?
        .visit(&mut dynamic_assumptions)?;
    let analyzed_program = dynamic_assumptions.analyzed_program;

    let mut fns = HashMap::new();
    let mut objects = HashMap::new();
    let mut enums = HashMap::new();
    let mut impls = HashMap::new();
    let mut obj_fns = HashMap::new();
    let mut globals = HashMap::new();

    for m in parsed_program.modules {
        for (_, f) in m.functions {
            fns.insert(f.module_ref.clone(), f);
        }

        for (_, f) in m.objects {
            objects.insert(f.module_ref.clone(), f);
        }

        for (_, e) in m.enums {
            enums.insert(e.module_ref.clone(), e);
        }

        for (id, i) in m.impls {
            for (name, fun) in &i.fns {
                obj_fns.insert((id, name.clone()), fun.clone());
            }

            impls.insert(id, i);
        }

        for (_, g) in m.globals {
            globals.insert(g.module_ref.clone(), g);
        }
    }

    let program_for_analysis = analyzed_program.clone();
    let analyzed_program = Rc::new(analyzed_program);

    let mut i = InstantiationAdapter {
        base_solver: TyckSolver::new(analyzed_program.clone()),
        program_for_analysis,
        analyzed_program,

        fns,
        objects,
        enums,
        impls,
        obj_fns,

        instantiated_fns: HashMap::new(),
        instantiated_object_fns: HashMap::new(),
        instantiated_impls: HashMap::new(),
        instantiated_objects: HashMap::new(),
        instantiated_enums: HashMap::new(),
        solved_impls: HashMap::new(),
        instantiated_types: HashSet::new(),
    };

    if main_finder.0.is_none() {
        return perror!("Cannot find main function.");
    }

    let main_fn = main_finder.0.unwrap();
    i.instantiate_function(&main_fn, &[])?;

    let mut instantiated_globals = HashMap::new();
    for (name, g) in globals {
        let g = i.process_simple(g, &[], &[])?;
        instantiated_globals.insert(name, g);
    }

    Ok(InstantiatedProgram {
        main_fn,
        instantiated_globals,
        instantiated_fns: unwrap_values(i.instantiated_fns),
        instantiated_object_fns: unwrap_values(i.instantiated_object_fns),
        instantiated_impls: unwrap_values(i.instantiated_impls),
        instantiated_objects: unwrap_values(i.instantiated_objects),
        instantiated_enums: unwrap_values(i.instantiated_enums),
        instantiated_types: i.instantiated_types,
    })
}

fn unwrap_values<K: Eq + Hash, V>(map: HashMap<K, Option<V>>) -> HashMap<K, V> {
    map.into_iter().map(|(k, v)| (k, v.unwrap())).collect()
}

impl InstantiationAdapter {
    fn instantiate_function(&mut self, name: &ModuleRef, generics: &[AstType]) -> PResult<()> {
        let sig = InstFunctionSignature(name.clone(), generics.to_vec());

        if self.instantiated_fns.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_fns.insert(sig.clone(), None);

        let ids = &self.analyzed_program.clone().analyzed_functions[name].generics;
        let f = self
            .process_simple(self.fns[name].clone(), ids, generics)
            .with_comment(|| format!("In function `{}`", name.full_name()))?;

        self.instantiated_fns.insert(sig, Some(f));

        Ok(())
    }

    fn instantiate_object(&mut self, name: &ModuleRef, generics: &[AstType]) -> PResult<()> {
        let sig = InstObjectSignature(name.clone(), generics.to_vec());

        if self.instantiated_objects.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_objects.insert(sig.clone(), None);

        let ids = &self.analyzed_program.clone().analyzed_objects[name].generics;
        let o = self
            .process_simple(self.objects[name].clone(), ids, generics)
            .with_comment(|| format!("In object `{}`", name.full_name()))?;

        self.instantiated_objects.insert(sig, Some(o));

        Ok(())
    }

    fn instantiate_enum(&mut self, name: &ModuleRef, generics: &[AstType]) -> PResult<()> {
        let sig = InstEnumSignature(name.clone(), generics.to_vec());

        if self.instantiated_enums.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_enums.insert(sig.clone(), None);

        let ids = &self.analyzed_program.clone().analyzed_enums[name].generics;
        let o = self
            .process_simple(self.enums[name].clone(), ids, generics)
            .with_comment(|| format!("In enum `{}`", name.full_name()))?;

        let mut r = self.solve_enum_representation(o)?;
        // NOTE: I don't *think* I need to do this...
        r.fields = r.fields.visit(self)?;

        self.instantiated_enums.insert(sig, Some(r));

        Ok(())
    }

    fn instantiate_impl(
        &mut self,
        span: Span,
        sig: &AstImplSignature,
        ty: &AstType,
        trt: Option<&AstTraitType>,
    ) -> PResult<()> {
        let id = sig.impl_id;
        let generics = &sig.generics;
        let sig = InstImplSignature(sig.impl_id, sig.generics.clone());

        if self.instantiated_impls.contains_key(&sig) {
            return Ok(());
        }

        let impl_info = &self.analyzed_program.clone().analyzed_impls[&id];

        match impl_info.kind {
            AnImplKind::Regular => {
                self.instantiated_impls.insert(sig.clone(), None);

                if let Some(trt) = trt {
                    let objective = TyckObjective::Impl(ty.clone(), trt.clone());

                    // Ensure that this is the only impl for this specific `<ty as trt>::...`
                    self.solved_impls
                        .insert(objective, sig.clone())
                        .as_not_expected(
                            self.impls[&id].name_span,
                            "impl",
                            "<conflicting types>",
                        )?;
                }

                let ids = &impl_info.generics;
                let mut instantiate = GenericsAdapter::new(ids, generics);

                let mut imp = self.impls[&id].clone().visit(&mut instantiate)?;

                // These will be instantiated later...
                imp.fns.clear();

                let imp = typecheck_impl(&self.base_solver, imp)?.visit(self)?;

                self.instantiated_impls.insert(sig, Some(imp));
            },
            AnImplKind::DynamicDispatch => {
                self.instantiated_impls.insert(sig.clone(), None);

                let trt =
                    trt.expect("ICE: We should always have a backing trait in a dynamic impl");

                // Get a generated impl from the trait/ty combination
                let imp = self.get_dyn_dispatch_impl(span, ty, trt)?;
                let imp = typecheck_impl(&self.base_solver, imp)?.visit(self)?;

                self.instantiated_impls.insert(sig, Some(imp));
            },
            AnImplKind::DynamicCoersion | AnImplKind::DynamicDowncast => {
                // Do nothing.
            },
            AnImplKind::Dummy =>
                unreachable!("ICE: Dummy impls should not leak into the Instantiation phase"),
        }

        Ok(())
    }

    fn instantiate_object_function(
        &mut self,
        span: Span,
        call_type: &AstType,
        trt: Option<&AstTraitType>,
        impl_sig: &AstImplSignature,
        fn_name: &str,
        fn_generics: &[AstType],
    ) -> PResult<()> {
        self.instantiate_impl(span, impl_sig, call_type, trt)?;

        debug!(
            "Instantiating <{:?} as {:?}>:{:?} with {:?} generics",
            call_type, trt, fn_name, fn_generics
        );

        let id = impl_sig.impl_id;
        let sig = InstObjectFunctionSignature(
            call_type.clone(),
            trt.cloned(),
            fn_name.into(),
            fn_generics.into(),
        );

        if self.instantiated_object_fns.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_object_fns.insert(sig.clone(), None);

        let impl_data = &self.analyzed_program.clone().analyzed_impls[&id];

        match impl_data.kind {
            AnImplKind::Regular => {
                let fn_data = &impl_data.methods[fn_name];

                let obj_ids = &impl_data.generics;
                let obj_generics = &impl_sig.generics;
                let fn_ids = &fn_data.generics;

                // Let's first instantiate it.
                let mut obj_instantiate = GenericsAdapter::new(obj_ids, obj_generics);
                let mut fn_instantiate = GenericsAdapter::new(fn_ids, fn_generics);
                let f = self.obj_fns[&(id, fn_name.into())]
                    .clone()
                    .visit(&mut obj_instantiate)?
                    .visit(&mut fn_instantiate)?;

                let f = typecheck_impl_fn(&self.base_solver, f, call_type, trt, fn_generics)?
                    .visit(self)?;
                self.instantiated_object_fns.insert(sig, Some(f));
            },
            AnImplKind::DynamicDispatch => {
                let trt =
                    trt.expect("ICE: We should always have a backing trait in a dynamic impl");

                let f = self.get_dyn_dispatch_function(span, call_type, trt, fn_name)?;
                let f = typecheck_impl_fn(&self.base_solver, f, call_type, Some(trt), fn_generics)?;

                self.instantiated_object_fns.insert(sig, Some(f));
            },
            AnImplKind::DynamicCoersion => {
                assert!(fn_name == "into");

                let trt =
                    trt.expect("ICE: We should always have a backing trait in a dynamic impl");

                self.instantiate_dynamic_object_functions(span, call_type, &trt.generics[0])?;

                let f = self.get_dyn_coersion_function(call_type, trt)?;
                let f = typecheck_impl_fn(&self.base_solver, f, call_type, Some(trt), fn_generics)?;

                self.instantiated_object_fns.insert(sig, Some(f));
            },
            AnImplKind::DynamicDowncast => {
                assert!(fn_name == "try_downcast");

                let trt =
                    trt.expect("ICE: We should always have a backing trait in a dynamic impl");

                let f = self.get_dyn_downcast_function(call_type, trt)?;
                let f = typecheck_impl_fn(&self.base_solver, f, call_type, Some(trt), fn_generics)?;

                self.instantiated_object_fns.insert(sig, Some(f));
            },
            AnImplKind::Dummy =>
                unreachable!("ICE: Dummy impls should not leak into the Instantiation phase"),
        }

        Ok(())
    }

    fn instantiate_dynamic_object_functions(
        &mut self,
        span: Span,
        obj_ty: &AstType,
        dyn_ty: &AstType,
    ) -> PResult<()> {
        if let AstType::DynamicType { trait_tys } = dyn_ty {
            for AstTraitTypeWithAssocs { trt, .. } in trait_tys {
                // First, we need to introspect how this obj->dyn relationship is satisfied.
                // Unfortunately, since we don't tag it anywhere during the process, we have to
                // do it manually.
                // TODO: We can cache this result.
                let trt_info = &self.analyzed_program.clone().analyzed_traits[&trt.name];

                let goal = TyckInstantiatedImpl::new(obj_ty.clone(), trt.clone());
                if let TyckInstantiatedImpl {
                    impl_signature: Some(impl_sig),
                    ..
                } = self.process_simple(goal, &[], &[])?
                {
                    // Instantiate all methods (i.e. has_self) with no generics
                    for (fn_name, fn_data) in &trt_info.methods {
                        if check_dyn_dispatchable(span, fn_data, trt, fn_name).is_err() {
                            continue;
                        }

                        self.instantiate_object_function(
                            span,
                            obj_ty,
                            Some(trt),
                            &impl_sig,
                            fn_name,
                            &[],
                        )?;
                    }
                } else {
                    unreachable!(
                        "ICE: Impl signature should be given after typechecking `{} :- {:?}`",
                        obj_ty, trt
                    );
                }
            }

            Ok(())
        } else {
            unreachable!(
                "ICE: Can only instantiate dynamic calls for Dyn type, got `{}`",
                dyn_ty
            )
        }
    }

    fn get_dyn_dispatch_impl(
        &mut self,
        span: Span,
        ty: &AstType,
        trt: &AstTraitType,
    ) -> PResult<AstImpl> {
        if let AstType::DynamicType { trait_tys } = ty {
            for AstTraitTypeWithAssocs {
                trt: dynamic_trt,
                assoc_bindings,
            } in trait_tys
            {
                if dynamic_trt == trt {
                    return analyze_item_inplace(
                        &mut self.program_for_analysis,
                        AstImpl::new(
                            span,
                            vec![],
                            Some(trt.clone()),
                            ty.clone(),
                            HashMap::new(),
                            vec![],
                            assoc_bindings
                                .iter()
                                .map(|(k, v)| (k.clone(), v.clone()))
                                .collect(),
                            true,
                        ),
                    );
                }
            }

            unreachable!(
                "ICE: We should have matched (exactly?) one trait type for the given dynamic type"
            );
        } else {
            unreachable!("ICE: Can only generate dynamic impl for a dynamic type");
        }
    }

    fn get_dyn_dispatch_function(
        &mut self,
        span: Span,
        ty: &AstType,
        trt: &AstTraitType,
        fn_name: &str,
    ) -> PResult<AstObjectFunction> {
        let method_info = &self.analyzed_program.analyzed_traits[&trt.name].methods[fn_name];

        check_dyn_dispatchable(span, &method_info, trt, fn_name)?;

        let (parameter_tys, return_ty, _) = instantiate_trait_fn_signature(
            &self.analyzed_program,
            &trt.name,
            &trt.generics,
            fn_name,
            &[],
            ty,
        )?;

        let parameter_list: Vec<_> = parameter_tys
            .into_iter()
            .enumerate()
            .map(|(i, p)| AstNamedVariable::new(span, format!("$dynamic{}", i), p))
            .collect();

        let mut dispatch_arguments: Vec<_> = parameter_list
            .iter()
            .map(|p| {
                InstructionArgument::Expression(AstExpression::quoted_identifier(
                    p.span,
                    p.name.clone(),
                    p.id,
                ))
            })
            .collect();

        let decorated_name = decorate_dynamic_fn(trt, fn_name)?;

        dispatch_arguments.insert(
            0,
            InstructionArgument::Expression(AstExpression::literal(
                span,
                AstLiteral::String(decorated_name),
            )),
        );

        let definition = AstExpression::instruction(
            span,
            "ch_dynamic_dispatch".to_string(),
            dispatch_arguments,
            InstructionOutput::Type(return_ty.clone()),
        );

        let f = AstObjectFunction::new(
            span,
            fn_name.to_string(),
            vec![],
            true,
            parameter_list,
            return_ty,
            vec![],
            Some(definition),
        );

        analyze_item_inplace(&mut self.program_for_analysis, f)
    }

    fn get_dyn_coersion_function(
        &mut self,
        ty: &AstType,
        trt: &AstTraitType,
    ) -> PResult<AstObjectFunction> {
        let dyn_ty = &trt.generics[0];
        let instruction_name = match ty {
            AstType::DynamicType { .. } => "ch_dynamic_transmute",
            _ => "ch_dynamic_box",
        };

        let mut i: AstImpl = cheshire_quote!(
            &mut self.program_for_analysis,
            "
            impl std::operators::lang::Into<{dyn_ty}> for <{concrete_ty}> {{
                fn into(self) -> {dyn_ty} {{
                    instruction {instruction} (self, _: Self, _: {dyn_ty}) -> {dyn_ty}
                }}
            }}",
            instruction = AstLiteral::String(instruction_name.to_string()),
            dyn_ty = dyn_ty,
            concrete_ty = ty,
        );

        Ok(i.fns.remove("into").unwrap())
    }

    fn get_dyn_downcast_function(
        &mut self,
        dyn_ty: &AstType,
        _: &AstTraitType,
    ) -> PResult<AstObjectFunction> {
        let mut i: AstImpl = cheshire_quote!(
            &mut self.program_for_analysis,
            "
            impl std::any::Downcast for <{dyn_ty}> {{
                fn try_downcast<_T>(self) -> std::option::Option<_T> {{
                    instruction \"ch_dynamic_unbox\" (self, _: _T) -> std::option::Option<_T>
                }}
            }}",
            dyn_ty = dyn_ty,
        );

        Ok(i.fns.remove("try_downcast").unwrap())
    }

    fn process_simple<T>(&mut self, t: T, ids: &[AstGeneric], tys: &[AstType]) -> PResult<T>
    where
        T: Visit<GenericsAdapter>
            + Visit<TyckSolver>
            + Visit<InstantiationAdapter>
            + for<'a> Visit<TypeAmbiguityAdapter<'a>>
            + Debug
            + Eq
            + Clone,
    {
        // Let's first instantiate it.
        let mut instantiate = GenericsAdapter::new(ids, tys);
        let t = t.visit(&mut instantiate)?;

        typecheck_simple(&self.base_solver, t)?.visit(self)
    }

    fn solve_enum_representation(&self, e: AstEnum) -> PResult<InstEnumRepresentation> {
        debug!("Enum {} is represented with ...", e.module_ref.full_name());

        let discriminants = e
            .variants
            .keys()
            .enumerate()
            .map(|(i, v)| (v.clone(), i as u64))
            .collect();

        let mut fields = vec![];
        let mut variants = HashMap::new();

        for (name, variant) in e.variants {
            let mut free_fields: HashMap<usize, AstType> =
                fields.iter().cloned().enumerate().collect();
            let mut member_idxes = vec![];

            for f in variant.fields {
                for t in self.flatten_ty(f)? {
                    // Try to find a type that matches this one in the free fields. If we can't,
                    // then add that field to the end of the representation. Maybe it'll get used
                    // by another variant.
                    let idx = self.find_field(&mut free_fields, t).left_or_else(|t| {
                        let idx = fields.len();
                        fields.push(t);
                        idx
                    }) + 1; // Add 1 to account for the later added discriminant at position 0.

                    member_idxes.push(idx);
                }
            }

            variants.insert(name, member_idxes);
        }

        fields.insert(0, AstType::Int); // Discriminant always goes first
        debug!(
            "{} fields: [{}]",
            fields.len(),
            fields
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<String>>()
                .join(", ")
        );

        for (n, d) in &discriminants {
            debug!("{}!{} -> {}", e.module_ref.full_name(), n, d);
        }

        Ok(InstEnumRepresentation {
            fields,
            variants,
            discriminants,
        })
    }

    fn flatten_ty(&self, t: AstType) -> PResult<Vec<AstType>> {
        match t {
            AstType::Tuple { types } => {
                let mut flat = Vec::new();

                for t in types {
                    flat.extend(self.flatten_ty(t)?);
                }

                Ok(flat)
            },
            AstType::Enum(name, generics) => {
                let sig = InstEnumSignature(name, generics);

                if let Some(instantiated_enum) = self.instantiated_enums[&sig].as_ref() {
                    Ok(instantiated_enum.fields.clone())
                } else {
                    let span = self.enums[&sig.0 /* == name */].name_span;
                    perror_at!(
                        span,
                        "Enum `{}` is self-referential, and therefore cannot be represented in \
                         memory!",
                        &AstType::Enum(sig.0 /* == name */, sig.1 /* == generics */)
                    )
                }
            },
            AstType::ClosureType { .. } => Ok(vec![
                AstType::fn_ptr_type(vec![], AstType::none()),
                AstType::Int,
                AstType::ClosureEnvType,
            ]),
            t => Ok(vec![t]),
        }
    }

    fn find_field(
        &self,
        f: &mut HashMap<usize, AstType>,
        seeking_ty: AstType,
    ) -> Either<usize, AstType> {
        let mut found = None; // Index in the tuple

        for (idx, ty) in f.iter() {
            if *ty == seeking_ty {
                found = Some(*idx);
                break;
            }
        }

        if let Some(i) = found {
            f.remove(&i);
        }

        match found {
            Some(idx) => Either::Left(idx),
            None => Either::Right(seeking_ty),
        }
    }
}

impl AstAdapter for InstantiationAdapter {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::Object(name, generics) => {
                self.instantiate_object(name, generics)?;
            },
            AstType::Enum(name, generics) => {
                self.instantiate_enum(name, generics)?;
            },
            _ => { /* Do nothing. */ },
        }

        self.instantiated_types.insert(t.clone());

        Ok(t)
    }

    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::FnCall {
                fn_name, generics, ..
            } => {
                self.instantiate_function(fn_name, generics)?;
            },
            AstExpressionData::StaticCall {
                call_type,
                associated_trait,
                impl_signature,
                fn_name,
                fn_generics,
                ..
            } => {
                let impl_sig = impl_signature.as_ref().unwrap();
                self.instantiate_object_function(
                    e.span,
                    call_type,
                    associated_trait.as_ref().map(|trt| &trt.trt),
                    impl_sig,
                    fn_name,
                    fn_generics,
                )?;
            },
            AstExpressionData::GlobalFn { name } => {
                self.instantiate_function(name, &[])?;
            },
            _ => { /* Do nothing. */ },
        }

        Ok(e)
    }
}

struct MainFinder(Option<ModuleRef>);

impl AstAdapter for MainFinder {
    fn enter_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        if &f.name == "main" {
            if self.0.is_some() {
                return perror_at!(
                    f.name_span,
                    "Duplicated `main` function: first found at `{}`, also found at `{}`",
                    self.0.as_ref().unwrap().full_name(),
                    f.module_ref.full_name()
                );
            }

            if !f.generics.is_empty() {
                return perror_at!(
                    f.name_span,
                    "Main function `{}` must have zero generics, found {}.",
                    f.module_ref.full_name(),
                    f.generics.len()
                );
            }

            if !f.parameter_list.is_empty() {
                return perror_at!(
                    f.name_span,
                    "Main function `{}` must have parameters, found {}.",
                    f.module_ref.full_name(),
                    f.parameter_list.len()
                );
            }

            if f.return_type.inner != AstType::Int {
                return perror_at!(
                    f.name_span,
                    "Main function `{}` must return `Int`.",
                    f.module_ref.full_name(),
                );
            }

            self.0 = Some(f.module_ref.clone());
        }

        Ok(f)
    }
}

impl TyckAdapter for InstantiationAdapter {}

fn check_dyn_dispatchable(
    span: Span,
    fun: &AnFunctionData,
    trt: &AstTraitType,
    fn_name: &str,
) -> PResult<()> {
    if !fun.has_self {
        return perror_at!(
            span,
            "Method `<_ as {}>::{}(...)` cannot be dynamically dispatched, must have `self` as \
             first parameter!",
            trt,
            fn_name
        );
    }

    if !fun.restrictions.is_empty() {
        return perror_at!(
            span,
            "Method `<_ as {}>::{}(...)` cannot be dynamically dispatched, must have no \
             restriction bounds.",
            trt,
            fn_name
        );
    }

    if !fun.generics.is_empty() {
        return perror_at!(
            span,
            "Method `<_ as {}>::{}(...)` cannot be dynamically dispatched, must have no generics.",
            trt,
            fn_name,
        );
    }

    let mut deny_self = DenySelfInDyn(trt);

    if deny_self.has_self(&fun.return_type) {
        return perror_at!(
            span,
            "Method `<_ as {}>::{}(...)` cannot be dynamically dispatched. Return type `{}` \
             cannot include `Self`.",
            trt,
            fn_name,
            &fun.return_type,
        );
    }

    for (idx, param) in fun.parameters[1..].iter().enumerate() {
        if deny_self.has_self(param) {
            return perror_at!(
                span,
                "Method `<_ as {}>::{}(...)` cannot be dynamically dispatched. Parameter #{} \
                 cannot include `Self`.",
                trt,
                fn_name,
                idx,
            );
        }
    }

    Ok(())
}

// Deny `Self`, except when in an associated type corresponding to the given
// trait.
struct DenySelfInDyn<'a>(&'a AstTraitType);

impl<'a> DenySelfInDyn<'a> {
    fn has_self(&mut self, ty: &AstType) -> bool {
        ty.clone().visit(self).is_err()
    }
}

impl<'a> AstAdapter for DenySelfInDyn<'a> {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            // Allow `<Self as Trait>::Name`.
            // This type is guaranteed not to flail around and change
            // depending on our target (Dyn or the concrete type)
            // due to the obligation that we have to fully specify
            // all associated types as part of the Dyn itself.
            AstType::AssociatedType {
                obj_ty,
                trait_ty:
                    Some(AstTraitTypeWithAssocs {
                        trt,
                        assoc_bindings,
                    }),
                name,
            } if &trt == self.0 => match *obj_ty {
                AstType::SelfType => {
                    let trait_ty = AstTraitTypeWithAssocs {
                        trt,
                        assoc_bindings,
                    };
                    trait_ty.visit(self)?;
                    Ok(AstType::none())
                },
                obj_ty => Ok(AstType::AssociatedType {
                    obj_ty: Box::new(obj_ty),
                    trait_ty: Some(AstTraitTypeWithAssocs {
                        trt,
                        assoc_bindings,
                    }),
                    name,
                }),
            },
            AstType::SelfType => perror!("The `Self` type is not allowed in this environment"),
            t => Ok(t),
        }
    }
}
