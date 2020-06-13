use crate::{
    ana::{
        represent::{AnImplData, AnalyzedProgram},
        represent_visitor::AnAdapter,
    },
    ast::{visitor::AstAdapter, *},
    tyck::{
        tyck_instantiation::*, TyckAdapter, TyckInstantiatedImpl, TyckInstantiatedObjectFunction,
        TYCK_MAX_DEPTH,
    },
    util::{IntoDisplay, PError, PResult, Visit, ZipExact, ZipKeys},
};
use display::DisplayTraitTypes;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct TyckSolver {
    program: Rc<AnalyzedProgram>,
    epochs: Vec<TyckEpoch>,

    // Temporary values
    return_type: Vec<AstType>,
    variables: HashMap<VariableId, AstType>,
    loops: HashMap<LoopId, AstType>,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum TyckObjective {
    Impl(AstType, AstTraitType),
    Method(AstType, String),
    AssociatedType(AstType, String),
    WellFormed(ModuleRef, Vec<AstType>),
}

#[derive(Debug, Clone)]
struct TyckEpoch {
    inferences: HashMap<InferId, AstType>,
    successes: HashMap<TyckObjective, Option<AstImplSignature>>,

    // Type solver iteration values
    ambiguous: Option<String>,
}

macro_rules! top_epoch {
    ($x:expr) => {
        $x.epochs.last().unwrap()
    };
}

macro_rules! top_epoch_mut {
    ($x:expr) => {
        $x.epochs.last_mut().unwrap()
    };
}

const INDENT: &'static str = "   ";

impl TyckSolver {
    pub fn new(analyzed_program: Rc<AnalyzedProgram>) -> TyckSolver {
        TyckSolver {
            program: analyzed_program,
            epochs: vec![TyckEpoch {
                inferences: hashmap! {},
                successes: hashmap! {},
                ambiguous: None,
            }],
            return_type: vec![],
            variables: hashmap! {},
            loops: hashmap! {},
            // TODO: I: I can memoize the async object name for compiler speedification
        }
    }

    pub fn typecheck_loop<T>(&mut self, mut t: T) -> PResult<T>
    where
        T: for<'a> Visit<TypeAmbiguityAdapter<'a>> + Visit<TyckSolver> + Clone + Eq + Debug,
    {
        debug!("===== ===== Start ze loop! ===== =====");
        debug!("{:#?}", t);

        for i in 0.. {
            debug!("===== LOOP ({}) =====", i);

            let new_t = t
                .clone()
                .visit(self)?
                .visit(&mut TypeAmbiguityAdapter(self))?;
            let epoch = top_epoch_mut!(self);

            if new_t == t {
                if let Some(ambiguity) = &epoch.ambiguous {
                    debug!("Yikes:");

                    for (x, y) in &epoch.inferences {
                        debug!("_{} => {}", x.0, y);
                    }

                    debug!("{:#?}", t);
                    return perror!("Tyck: {}", ambiguity);
                } else {
                    debug!("End ze loop!\n{:#?}", t);
                    return Ok(t);
                }
            }

            t = new_t;
            epoch.ambiguous = None;
        }

        unreachable!()
    }

    fn unify(&mut self, full_unify: bool, a: &AstType, b: &AstType) -> PResult<()> {
        let (old_a, old_b) = (a, b);
        let (a, b) = (self.normalize_ty(a.clone())?, self.normalize_ty(b.clone())?);

        if a == b {
            trace!(
                "{}Unifying {} and {} (pre-norm {} and {}) (FULL? {})",
                INDENT.repeat(self.epochs.len()),
                a,
                b,
                old_a,
                old_b,
                full_unify
            );
        } else {
            debug!(
                "{}Unifying {} and {} (pre-norm {} and {}) (FULL? {})",
                INDENT.repeat(self.epochs.len()),
                a,
                b,
                old_a,
                old_b,
                full_unify
            );
        }

        match (a, b) {
            /* Generics should have been repl'ed out. */
            (AstType::Generic(..), _)
            | (_, AstType::Generic(..))
            | (AstType::GenericPlaceholder(..), _)
            | (_, AstType::GenericPlaceholder(..))
            | (AstType::ObjectEnum(..), _)
            | (_, AstType::ObjectEnum(..)) => unreachable!(),

            (AstType::AssociatedType { .. }, _) | (_, AstType::AssociatedType { .. }) => {
                // Can't do nothin'. At least make sure the normalization process
                // marked this as ambiguous.
                assert!(top_epoch_mut!(self).ambiguous.is_some());
            },

            (AstType::Infer(lid), AstType::Infer(rid)) if lid == rid => {
                /* Do nothing. No cycles in this house. */
            },
            (AstType::Infer(id), ty) => {
                if top_epoch_mut!(self)
                    .inferences
                    .insert(id, ty.clone())
                    .is_some()
                {
                    unreachable!(
                        "ICE: Duplicated inference, normalization should not let this happen!"
                    );
                }
            },

            (ty, AstType::Infer(id)) => {
                if !full_unify && top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous =
                        Some(format!("Ambiguous unify: {} <- {}", ty, AstType::Infer(id)));
                }

                if full_unify {
                    if top_epoch_mut!(self)
                        .inferences
                        .insert(id, ty.clone())
                        .is_some()
                    {
                        unreachable!(
                            "ICE: Duplicated inference, normalization should not let this happen!"
                        );
                    }
                }
            },

            (AstType::Int, AstType::Int)
            | (AstType::Float, AstType::Float)
            | (AstType::Char, AstType::Char)
            | (AstType::Bool, AstType::Bool)
            | (AstType::String, AstType::String)
            | (AstType::ClosureEnvType, AstType::ClosureEnvType) => {},

            (AstType::DummyGeneric(a, ..), AstType::DummyGeneric(b, ..)) if a == b => {},
            (AstType::Dummy(a), AstType::Dummy(b)) if a == b => {},

            (AstType::Array { ty: a_ty }, AstType::Array { ty: b_ty }) => {
                self.unify(full_unify, &a_ty, &b_ty)?;
            },

            (AstType::Tuple { types: a_tys }, AstType::Tuple { types: b_tys }) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "tuple types")? {
                    self.unify(full_unify, &a_ty, &b_ty)?;
                }
            },

            (AstType::Object(a_name, a_tys), AstType::Object(b_name, b_tys)) => {
                if a_name != b_name {
                    return perror!(
                        "Cannot equate distinct objects: {} and {}",
                        a_name.full_name(),
                        b_name.full_name()
                    );
                } else {
                    for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "object generics")? {
                        self.unify(full_unify, &a_ty, &b_ty)?;
                    }
                }
            },

            (AstType::Enum(a_name, a_tys), AstType::Enum(b_name, b_tys)) =>
                if a_name != b_name {
                    return perror!(
                        "Cannot equate distinct enums: {} and {}",
                        a_name.full_name(),
                        b_name.full_name()
                    );
                } else {
                    for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "enum generics")? {
                        self.unify(full_unify, &a_ty, &b_ty)?;
                    }
                },

            (
                AstType::ClosureType {
                    args: a_args,
                    ret_ty: a_ret,
                },
                AstType::ClosureType {
                    args: b_args,
                    ret_ty: b_ret,
                },
            ) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_args, b_args, "closure arguments")? {
                    self.unify(full_unify, &a_ty, &b_ty)?;
                }

                self.unify(full_unify, &a_ret, &b_ret)?;
            },

            (
                AstType::FnPointerType {
                    args: a_args,
                    ret_ty: a_ret,
                },
                AstType::FnPointerType {
                    args: b_args,
                    ret_ty: b_ret,
                },
            ) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_args, b_args, "closure arguments")? {
                    self.unify(full_unify, &a_ty, &b_ty)?;
                }

                self.unify(full_unify, &a_ret, &b_ret)?;
            },

            (
                AstType::DynamicType {
                    trait_tys: a_trait_tys,
                },
                AstType::DynamicType {
                    trait_tys: b_trait_tys,
                },
            ) => {
                self.unify_dynamic_tys(full_unify, &a_trait_tys, &b_trait_tys)?;
            },

            (a, b) => {
                return perror!("Type non-union, {} and {}", a, b);
            },
        }

        Ok(())
    }

    fn unify_dynamic_tys(
        &mut self,
        full_unify: bool,
        a_trait_tys: &BTreeSet<AstTraitTypeWithAssocs>,
        b_trait_tys: &BTreeSet<AstTraitTypeWithAssocs>,
    ) -> PResult<()> {
        let a_trait_refs = Self::dedupe_trait_refs(a_trait_tys);
        let b_trait_refs = Self::dedupe_trait_refs(b_trait_tys);

        for (_, (a_generics, b_generics)) in ZipKeys::zip_keys(a_trait_refs, b_trait_refs) {
            self.unify_all(full_unify, &a_generics, &b_generics)?;
        }

        let a_trait_map: HashMap<_, _> = self
            .normalize_trts(a_trait_tys)?
            .into_iter()
            .map(|x| (x.trt.clone(), x.assoc_bindings.clone()))
            .collect();
        let b_trait_map: HashMap<_, _> = self
            .normalize_trts(b_trait_tys)?
            .into_iter()
            .map(|x| (x.trt.clone(), x.assoc_bindings.clone()))
            .collect();

        if a_trait_map.len() != b_trait_map.len() {
            return perror!(
                "Dynamic trait non-union, Dyn<{}> and Dyn<{}>",
                DisplayTraitTypes(self.normalize_trts(a_trait_tys)?.iter()),
                DisplayTraitTypes(self.normalize_trts(b_trait_tys)?.iter())
            );
        }

        for (b_trt, b_assoc_bindings) in b_trait_map {
            if let Some(a_assoc_bindings) = a_trait_map.get(&b_trt) {
                for (_, (a_ty, b_ty)) in ZipKeys::zip_keys(a_assoc_bindings, b_assoc_bindings) {
                    self.unify(full_unify, a_ty, &b_ty)?;
                }
            } else {
                return perror!(
                    "Dynamic trait non-union, Dyn<{}> and Dyn<{}>",
                    DisplayTraitTypes(self.normalize_trts(a_trait_tys)?.iter()),
                    DisplayTraitTypes(self.normalize_trts(b_trait_tys)?.iter())
                );
            }
        }

        Ok(())
    }

    fn dedupe_trait_refs(
        trait_tys: &BTreeSet<AstTraitTypeWithAssocs>,
    ) -> HashMap<ModuleRef, Vec<AstType>> {
        let mut unique_traits = hashmap! {};
        let mut holdout_traits = HashSet::new();

        for AstTraitTypeWithAssocs { trt, .. } in trait_tys {
            if holdout_traits.contains(&trt.name) {
                unique_traits.remove(&trt.name);
            } else {
                unique_traits.insert(trt.name.clone(), trt.generics.clone());
                holdout_traits.insert(trt.name.clone());
            }
        }

        unique_traits
    }

    fn unify_all(&mut self, full_unify: bool, a: &[AstType], b: &[AstType]) -> PResult<()> {
        assert_eq!(a.len(), b.len());

        for (a, b) in Iterator::zip(a.iter(), b.iter()) {
            self.unify(full_unify, a, b)?;
        }

        Ok(())
    }

    fn unify_pattern(
        &mut self,
        full_unify: bool,
        pattern: &AstMatchPattern,
        other_ty: &AstType,
    ) -> PResult<()> {
        self.unify(true, &pattern.ty, other_ty)?;

        match &pattern.data {
            AstMatchPatternData::Underscore => {},
            AstMatchPatternData::Identifier(v) => self.unify(full_unify, &v.ty, other_ty)?,
            AstMatchPatternData::Tuple(children) => {
                let children_tys = children.iter().map(|child| child.ty.clone()).collect();
                self.unify(full_unify, &AstType::tuple(children_tys), other_ty)?;
            },
            AstMatchPatternData::Literal(lit) => match lit {
                AstLiteral::True | AstLiteral::False =>
                    self.unify(full_unify, &AstType::Bool, other_ty)?,
                AstLiteral::Int(..) => self.unify(full_unify, &AstType::Int, other_ty)?,
                AstLiteral::Float(..) => self.unify(full_unify, &AstType::Float, other_ty)?,
                AstLiteral::Char(..) => self.unify(full_unify, &AstType::Char, other_ty)?,
                AstLiteral::String { .. } => self.unify(full_unify, &AstType::String, other_ty)?,
            },
            AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ..
            } => {
                let expected_tys =
                    instantiate_enum_pattern(&self.program, enumerable, &generics, &variant)?;

                for (child, ty) in
                    ZipExact::zip_exact(children, expected_tys, "positional elements")?
                {
                    self.unify_pattern(full_unify, child, &ty)?;
                }

                self.unify(
                    full_unify,
                    &other_ty,
                    &AstType::enumerable(enumerable.clone(), generics.clone()),
                )?;
            },
            AstMatchPatternData::PlainEnum { .. } | AstMatchPatternData::NamedEnum { .. } =>
                unreachable!(),
        }

        Ok(())
    }

    fn commit_epoch(&mut self) -> PResult<()> {
        let inferences = top_epoch!(self).inferences.clone().visit(self)?;

        let mut successes = hashmap! {};
        for (obj, imp) in top_epoch!(self).successes.clone() {
            let obj = match obj {
                TyckObjective::Impl(ty, trt) =>
                    TyckObjective::Impl(ty.visit(self)?, trt.visit(self)?),
                TyckObjective::Method(ty, name) => TyckObjective::Method(ty.visit(self)?, name),
                TyckObjective::AssociatedType(ty, name) =>
                    TyckObjective::AssociatedType(ty.visit(self)?, name),
                TyckObjective::WellFormed(name, tys) =>
                    TyckObjective::WellFormed(name, tys.visit(self)?),
            };

            let imp = imp.visit(self)?;

            if let Some(other_imp) = successes.get(&obj) {
                if imp != *other_imp {
                    return perror!("Conflict impls.... TODO: better message",);
                }
            } else {
                successes.insert(obj, imp);
            }
        }

        let epoch = self.epochs.pop().unwrap();
        let top = top_epoch_mut!(self);

        // Copy pre-normalized values over.
        top.successes = successes;
        top.inferences = inferences;

        // Inherit ambiguity
        if top.ambiguous.is_none() {
            top.ambiguous = epoch.ambiguous;
        }

        Ok(())
    }

    fn rollback_epoch(&mut self) -> TyckEpoch {
        self.epochs.pop().unwrap_or_else(|| {
            unreachable!("ICE: Cannot roll back when there are no epochs on the stack!")
        })
    }

    fn push_new_epoch(&mut self) {
        if self.epochs.len() > TYCK_MAX_DEPTH {
            // TODO: Should this just be a perror?
            unreachable!("ICE: OVERFLOW");
        }

        let top = top_epoch!(self);
        let new = TyckEpoch {
            inferences: top.inferences.clone(),
            successes: top.successes.clone(),
            ambiguous: None,
        };

        self.epochs.push(new);
    }

    fn push_epoch(&mut self, t: TyckEpoch) {
        self.epochs.push(t);
    }

    fn satisfy_impl(
        &mut self,
        ty: &AstType,
        trt: &AstTraitTypeWithAssocs,
    ) -> PResult<Option<AstImplSignature>> {
        let key = TyckObjective::Impl(ty.clone(), trt.trt.clone());

        if let Some(Some(imp)) = top_epoch!(self).successes.get(&key) {
            return Ok(Some(imp.clone()));
        }

        debug!(
            "{}Satisfying {} :- {}",
            INDENT.repeat(self.epochs.len()),
            ty,
            trt
        );

        let trait_data = &self.program.clone().analyzed_traits[&trt.trt.name];
        let mut solutions = vec![];

        for imp in &trait_data.impls {
            let impl_generics: Vec<_> = self.program.analyzed_impls[imp]
                .generics
                .iter()
                .map(|_| AstType::infer())
                .collect();

            self.push_new_epoch();
            let sol = self.elaborate_impl(*imp, &impl_generics, ty, Some(&trt.trt));
            let epoch = self.rollback_epoch();

            if let Err(e) = &sol {
                debug!("{}... Err: {}", INDENT.repeat(self.epochs.len()), e.why());
            } else if let Some(a) = &epoch.ambiguous {
                debug!("{}... Amb: {}", INDENT.repeat(self.epochs.len()), a);
            } else {
                debug!("{}... Ok!", INDENT.repeat(self.epochs.len()));
            }

            solutions.push((sol, epoch));
        }

        match self.internal_disambiguate(solutions) {
            Ok(Some(imp)) => {
                let (a, b, c) =
                    instantiate_impl_signature(&self.program, imp.impl_id, &imp.generics, ty)?;
                debug!(
                    "{}>Ok impl {:?} for {:?} where {:?}",
                    INDENT.repeat(self.epochs.len()),
                    b,
                    a,
                    c
                );
                // Make sure we finally apply the trait bindings!
                for (name, bound_ty) in &trt.assoc_bindings {
                    self.unify(
                        true,
                        &instantiate_associated_ty(
                            &self.program,
                            imp.impl_id,
                            &imp.generics,
                            name,
                            ty,
                        )?,
                        bound_ty,
                    )?;
                }

                top_epoch_mut!(self)
                    .successes
                    .insert(key, Some(imp.clone()));
                Ok(Some(imp))
            },
            Ok(None) => {
                debug!("{}>Ambig", INDENT.repeat(self.epochs.len()),);
                if top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous = Some(format!(
                        "Ambiguous impl `{}` for `{}`",
                        self.normalize_trt(trt.clone())?,
                        self.normalize_ty(ty.clone())?
                    ));
                }
                Ok(None)
            },
            Err(_) => {
                debug!("{}>Err", INDENT.repeat(self.epochs.len()),);
                perror!(
                    "No suitable solution for impl `{}` for `{}`",
                    self.normalize_trt(trt.clone())?,
                    self.normalize_ty(ty.clone())?
                )
            },
        }
    }

    fn elaborate_impl(
        &mut self,
        impl_id: ImplId,
        impl_generics: &[AstType],
        ty: &AstType,
        trt: Option<&AstTraitType>,
    ) -> PResult<AstImplSignature> {
        let impl_info = &self.program.clone().analyzed_impls[&impl_id];
        debug!(
            "{}Trying impl {} for {} where {:?}",
            INDENT.repeat(self.epochs.len()),
            impl_info.trait_ty.as_ref().display(),
            impl_info.impl_ty,
            impl_info.restrictions
        );

        let (expected_ty, expected_trt, restrictions) =
            instantiate_impl_signature(&self.program, impl_id, impl_generics, ty)?;

        self.unify(false, &expected_ty, ty)?;

        match (&expected_trt, trt) {
            (Some(expected_trt), Some(trt)) =>
                self.unify_all(false, &expected_trt.generics, &trt.generics)?,
            (Some(_), None) | (None, Some(_)) => {
                unreachable!(
                    "ICE: Tried to unify an expected trait with no given trait. This should NEVER \
                     happen."
                );
            },
            (None, None) => {},
        }

        if top_epoch!(self).ambiguous.is_none() {
            self.satisfy_restrictions(&restrictions)?;
        }

        Ok(AstImplSignature {
            impl_id,
            generics: self.normalize_tys(impl_generics)?,
        })
    }

    fn satisfy_associated_type(
        &mut self,
        ty: &AstType,
        name: &str,
    ) -> PResult<Option<AstImplSignature>> {
        let key = TyckObjective::AssociatedType(ty.clone(), name.to_owned());

        if let Some(Some(imp)) = top_epoch!(self).successes.get(&key) {
            return Ok(Some(imp.clone()));
        }

        debug!(
            "{}Satisfying {} assoc ty {}",
            INDENT.repeat(self.epochs.len()),
            ty,
            name
        );

        let mut solutions = vec![];
        for trt in &self.program.clone().methods_to_traits[name] {
            let trait_data = &self.program.clone().analyzed_traits[&trt];

            for imp in &trait_data.impls {
                let impl_generics: Vec<_> = self.program.analyzed_impls[imp]
                    .generics
                    .iter()
                    .map(|_| AstType::infer())
                    .collect();

                self.push_new_epoch();
                let sol = self.elaborate_impl(
                    *imp,
                    &impl_generics,
                    ty,
                    Some(
                        &instantiate_impl_trait_ty(&self.program, *imp, &impl_generics, ty)?
                            .unwrap()
                            .trt,
                    ),
                );
                let epoch = self.rollback_epoch();

                if let Err(e) = &sol {
                    debug!("{}... Err: {}", INDENT.repeat(self.epochs.len()), e.why());
                } else if let Some(a) = &epoch.ambiguous {
                    debug!("{}... Amb: {}", INDENT.repeat(self.epochs.len()), a);
                } else {
                    debug!("{}... Ok!", INDENT.repeat(self.epochs.len()));
                }

                solutions.push((sol, epoch));
            }
        }

        match self.internal_disambiguate(solutions) {
            Ok(Some(imp)) => {
                let (a, b, c) =
                    instantiate_impl_signature(&self.program, imp.impl_id, &imp.generics, ty)?;
                debug!(
                    "{}>Ok assoc = impl {:?} for {:?} where {:?}",
                    INDENT.repeat(self.epochs.len()),
                    b,
                    a,
                    c
                );
                top_epoch_mut!(self)
                    .successes
                    .insert(key, Some(imp.clone()));
                Ok(Some(imp))
            },
            Ok(None) => {
                debug!("{}>Ambig assoc", INDENT.repeat(self.epochs.len()),);
                if top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous =
                        Some(format!("Ambiguous associated type `<{}>::{}`", ty, name));
                }
                Ok(None)
            },
            Err(_) => {
                debug!("{}>Err assoc", INDENT.repeat(self.epochs.len()),);
                perror!(
                    "No suitable trait for associated type `<{}>::{}`",
                    self.normalize_ty(ty.clone())?,
                    name
                )
            },
        }
    }

    fn satisfy_method(
        &mut self,
        ty: &AstType,
        name: &str,
        generics: &[AstType],
        arg_tys: &[AstType],
        return_ty: &AstType,
    ) -> PResult<Option<(Option<AstTraitTypeWithAssocs>, AstImplSignature)>> {
        let key = TyckObjective::Method(ty.clone(), name.to_owned());

        if let Some(Some(imp)) = top_epoch!(self).successes.get(&key) {
            return Ok(Some((
                instantiate_impl_trait_ty(&self.program, imp.impl_id, &imp.generics, ty)?,
                imp.clone(),
            )));
        }

        debug!(
            "{}Satisfying {} method {}",
            INDENT.repeat(self.epochs.len()),
            ty,
            name
        );

        let mut solutions = vec![];

        if let Some(traits) = self.program.clone().methods_to_traits.get(name) {
            for trt in traits {
                let trait_data = &self.program.clone().analyzed_traits[&trt];
                let fn_data = &trait_data.methods[name];

                if fn_data.parameters.len() != arg_tys.len() {
                    continue;
                }

                let fn_generics: Vec<_> = if generics.len() == 0 {
                    fn_data.generics.iter().map(|_| AstType::infer()).collect()
                } else if generics.len() == fn_data.generics.len() {
                    generics.to_vec()
                } else {
                    continue;
                };

                for imp in &trait_data.impls {
                    let impl_generics: Vec<_> = self.program.analyzed_impls[imp]
                        .generics
                        .iter()
                        .map(|_| AstType::infer())
                        .collect();

                    self.push_new_epoch();
                    let sol = self.elaborate_method(
                        *imp,
                        &impl_generics,
                        name,
                        &fn_generics,
                        arg_tys,
                        return_ty,
                        ty,
                        None,
                    );
                    let epoch = self.rollback_epoch();

                    if let Err(e) = &sol {
                        debug!("{}... Err: {}", INDENT.repeat(self.epochs.len()), e.why());
                    } else if let Some(a) = &epoch.ambiguous {
                        debug!("{}... Amb: {}", INDENT.repeat(self.epochs.len()), a);
                    } else {
                        debug!("{}... Ok!", INDENT.repeat(self.epochs.len()));
                    }

                    solutions.push((sol, epoch));
                }
            }
        }

        if let Some(impls) = self.program.clone().methods_to_anonymous_impls.get(name) {
            for imp in impls {
                let impl_data = &self.program.clone().analyzed_impls[imp];
                let fn_data = &impl_data.methods[name];

                if fn_data.parameters.len() != arg_tys.len() {
                    continue;
                }

                let fn_generics: Vec<_> = if generics.len() == 0 {
                    fn_data.generics.iter().map(|_| AstType::infer()).collect()
                } else if generics.len() == fn_data.generics.len() {
                    generics.to_vec()
                } else {
                    continue;
                };

                let impl_generics: Vec<_> = self.program.analyzed_impls[imp]
                    .generics
                    .iter()
                    .map(|_| AstType::infer())
                    .collect();

                self.push_new_epoch();
                let sol = self.elaborate_method(
                    *imp,
                    &impl_generics,
                    name,
                    &fn_generics,
                    arg_tys,
                    return_ty,
                    ty,
                    None,
                );
                let epoch = self.rollback_epoch();

                if let Err(e) = &sol {
                    debug!("{}... Err: {}", INDENT.repeat(self.epochs.len()), e.why());
                } else if let Some(a) = &epoch.ambiguous {
                    debug!("{}... Amb: {}", INDENT.repeat(self.epochs.len()), a);
                } else {
                    debug!("{}... Ok!", INDENT.repeat(self.epochs.len()));
                }

                solutions.push((sol, epoch));
            }
        }

        match self.internal_disambiguate(solutions) {
            Ok(Some(imp)) => {
                top_epoch_mut!(self)
                    .successes
                    .insert(key, Some(imp.clone()));

                Ok(Some((
                    instantiate_impl_trait_ty(&self.program, imp.impl_id, &imp.generics, ty)?,
                    imp,
                )))
            },
            Ok(None) => {
                if top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous =
                        Some(format!("Ambiguous method `<{}>:{}(...)`", ty, name));
                }
                Ok(None)
            },
            Err(_) => perror!(
                "No suitable trait for method `<{}>:{}(...)`",
                self.normalize_ty(ty.clone())?,
                name
            ),
        }
    }

    fn satisfy_method_with_trait(
        &mut self,
        ty: &AstType,
        trt: &AstTraitTypeWithAssocs,
        name: &str,
        fn_generics: &[AstType],
        arg_tys: &[AstType],
        return_ty: &AstType,
    ) -> PResult<Option<AstImplSignature>> {
        let key = TyckObjective::Impl(ty.clone(), trt.trt.clone());

        if let Some(Some(imp)) = top_epoch!(self).successes.get(&key) {
            return Ok(Some(imp.clone()));
        }

        debug!(
            "{}Satisfying {} :- {} (METHOD)",
            INDENT.repeat(self.epochs.len()),
            ty,
            trt
        );

        let mut solutions = vec![];

        let trait_data = &self.program.clone().analyzed_traits[&trt.trt.name];

        for imp in &trait_data.impls {
            let impl_generics: Vec<_> = self.program.analyzed_impls[imp]
                .generics
                .iter()
                .map(|_| AstType::infer())
                .collect();

            self.push_new_epoch();
            let sol = self.elaborate_method(
                *imp,
                &impl_generics,
                name,
                &fn_generics,
                arg_tys,
                return_ty,
                ty,
                Some(&trt.trt),
            );
            let epoch = self.rollback_epoch();

            if let Err(e) = &sol {
                debug!("{}... Err: {}", INDENT.repeat(self.epochs.len()), e.why());
            } else if let Some(a) = &epoch.ambiguous {
                debug!("{}... Amb: {}", INDENT.repeat(self.epochs.len()), a);
            } else {
                debug!("{}... Ok!", INDENT.repeat(self.epochs.len()));
            }

            solutions.push((sol, epoch));
        }

        match self.internal_disambiguate(solutions) {
            Ok(Some(imp)) => {
                // Make sure we finally apply the trait bindings!
                for (name, bound_ty) in &trt.assoc_bindings {
                    self.unify(
                        true,
                        &instantiate_associated_ty(
                            &self.program,
                            imp.impl_id,
                            &imp.generics,
                            name,
                            ty,
                        )?,
                        bound_ty,
                    )?;
                }

                top_epoch_mut!(self)
                    .successes
                    .insert(key, Some(imp.clone()));

                Ok(Some(imp))
            },
            Ok(None) => {
                if top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous =
                        Some(format!("Ambiguous method `<{}>:{}(...)`", ty, name));
                }
                Ok(None)
            },
            Err(_) => perror!(
                "No suitable solution for method `<{} as {}>:{}(...)`",
                self.normalize_ty(ty.clone())?,
                self.normalize_trt(trt.clone())?,
                name
            ),
        }
    }

    fn elaborate_method(
        &mut self,
        impl_id: ImplId,
        impl_generics: &[AstType],
        fn_name: &str,
        fn_generics: &[AstType],
        fn_args: &[AstType],
        return_ty: &AstType,
        call_ty: &AstType,
        call_trt: Option<&AstTraitType>,
    ) -> PResult<AstImplSignature> {
        let impl_info = &self.program.clone().analyzed_impls[&impl_id];
        debug!(
            "{}Trying impl {:?} for {:?} where {:?}",
            INDENT.repeat(self.epochs.len()),
            impl_info.trait_ty,
            impl_info.impl_ty,
            impl_info.restrictions
        );

        let (expected_call_ty, expected_trt, restrictions) =
            instantiate_impl_signature(&self.program, impl_id, impl_generics, call_ty)?;
        self.unify(false, &expected_call_ty, call_ty)?;

        match (&expected_trt, call_trt) {
            (Some(expected_trt), Some(trt)) =>
                self.unify_all(false, &expected_trt.generics, &trt.generics)?,
            (Some(_), None) => { /* This is okay! */ },
            (None, Some(_)) => {
                unreachable!(
                    "ICE: Tried to unify an expected trait with no given trait. This should NEVER \
                     happen."
                );
            },
            (None, None) => {},
        }

        if impl_info.is_regular() {
            let (expected_args, expected_return_ty, _) = instantiate_impl_fn_signature(
                &self.program,
                impl_id,
                impl_generics,
                fn_name,
                fn_generics,
                call_ty,
            )?;
            self.unify_all(true, &expected_args, fn_args)?;
            self.unify(true, &expected_return_ty, return_ty)?;
        }

        if top_epoch!(self).ambiguous.is_none() {
            self.satisfy_restrictions(&restrictions)?;
        }

        Ok(AstImplSignature {
            impl_id,
            generics: self.normalize_tys(impl_generics)?,
        })
    }

    fn internal_disambiguate(
        &mut self,
        solutions: Vec<(PResult<AstImplSignature>, TyckEpoch)>,
    ) -> PResult<Option<AstImplSignature>> {
        let mut solution: Option<(AstImplSignature, TyckEpoch)> = None;

        for (result, epoch) in solutions {
            if let Ok(signature) = result {
                if epoch.ambiguous.is_some() {
                    // Bail because this solution is ambiguous!
                    // Therefore, we need to continue to iterate until
                    // this solution becomes successful or an error.
                    return Ok(None);
                }

                solution = match (solution, (signature, epoch)) {
                    (None, new_solution) => Some(new_solution),
                    (Some((dummy_impl, _)), other) | (Some(other), (dummy_impl, _))
                        if self.program.analyzed_impls[&dummy_impl.impl_id].is_dummy() =>
                        Some(other),
                    (Some((dynamic_impl, _)), other) | (Some(other), (dynamic_impl, _))
                        if self.program.analyzed_impls[&dynamic_impl.impl_id]
                            .is_dynamic_dispatch() =>
                        Some(other),
                    (Some((dynamic_impl, _)), other) | (Some(other), (dynamic_impl, _))
                        if self.program.analyzed_impls[&dynamic_impl.impl_id].is_dynamic_cast() =>
                        Some(other),
                    (Some(_), _) => {
                        return perror!("Conflicting solutions!");
                    },
                };
            } else {
                // We can successfully get rid of the Error solution.
            }
        }

        if let Some((signature, epoch)) = solution {
            self.push_epoch(epoch);
            self.commit_epoch()?;

            Ok(Some(signature))
        } else {
            perror!("No solutions!")
        }
    }

    fn satisfy_restrictions(&mut self, restrictions: &[AstTypeRestriction]) -> PResult<bool> {
        let mut totally_satisfied = true;

        for AstTypeRestriction { ty, trt } in restrictions {
            totally_satisfied &= self.satisfy_impl(ty, trt)?.is_some();
        }

        Ok(totally_satisfied)
    }

    fn satisfy_well_formed_object(
        &mut self,
        name: &ModuleRef,
        generics: &[AstType],
    ) -> PResult<()> {
        let key = TyckObjective::WellFormed(name.clone(), generics.to_vec());
        if top_epoch!(self).successes.contains_key(&key) {
            return Ok(());
        }

        debug!(
            "{}Satisfying {} is well formed",
            INDENT.repeat(self.epochs.len()),
            AstType::Object(name.clone(), generics.to_vec())
        );

        if self.satisfy_restrictions(&instantiate_object_restrictions(
            &self.program,
            name,
            generics,
        )?)? {
            top_epoch_mut!(self).successes.insert(key, None);
        }

        Ok(())
    }

    fn satisfy_well_formed_enum(&mut self, name: &ModuleRef, generics: &[AstType]) -> PResult<()> {
        let key = TyckObjective::WellFormed(name.clone(), generics.to_vec());
        if top_epoch!(self).successes.contains_key(&key) {
            return Ok(());
        }

        debug!(
            "{}Satisfying {} is well formed",
            INDENT.repeat(self.epochs.len()),
            AstType::Enum(name.clone(), generics.to_vec())
        );

        if self.satisfy_restrictions(&instantiate_enum_restrictions(
            &self.program,
            name,
            generics,
        )?)? {
            top_epoch_mut!(self).successes.insert(key, None);
        }

        Ok(())
    }

    fn normalize_ty(&mut self, t: AstType) -> PResult<AstType> {
        t.visit(&mut NormalizationAdapter(self))
    }

    fn normalize_tys(&mut self, tys: &[AstType]) -> PResult<Vec<AstType>> {
        let mut ret_tys = vec![];

        for t in tys {
            ret_tys.push(self.normalize_ty(t.clone())?);
        }

        Ok(ret_tys)
    }

    fn normalize_trt(&mut self, t: AstTraitTypeWithAssocs) -> PResult<AstTraitTypeWithAssocs> {
        t.visit(&mut NormalizationAdapter(self))
    }

    fn normalize_trts(
        &mut self,
        trts: &BTreeSet<AstTraitTypeWithAssocs>,
    ) -> PResult<BTreeSet<AstTraitTypeWithAssocs>> {
        let mut ret_trts = BTreeSet::new();

        for t in trts {
            ret_trts.insert(self.normalize_trt(t.clone())?);
        }

        Ok(ret_trts)
    }
}

impl AstAdapter for TyckSolver {
    fn enter_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.satisfy_restrictions(&f.restrictions)?;

        self.return_type.push(f.return_type.inner.clone());
        self.variables = f
            .scope
            .as_ref()
            .unwrap()
            .iter()
            .map(|(_, v)| (v.id, v.ty.clone()))
            .collect();

        if let Some(block) = &f.definition {
            self.unify(true, &f.return_type.inner, &block.ty)?;
        }

        Ok(f)
    }

    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::While { id, .. } => {
                self.loops.insert(*id, e.ty.clone());
            },
            AstExpressionData::Closure {
                scope, return_ty, ..
            } => {
                self.return_type.push(return_ty.clone());
                self.variables.extend(
                    scope
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|(_, v)| (v.id, v.ty.clone())),
                );
            },
            AstExpressionData::Async {
                scope, return_ty, ..
            } => {
                self.return_type.push(return_ty.clone());
                self.variables.extend(
                    scope
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|(_, v)| (v.id, v.ty.clone())),
                );
            },
            _ => {},
        }

        Ok(e)
    }

    fn enter_ast_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        if let Some(scope) = &b.scope {
            self.variables
                .extend(scope.iter().map(|(_, v)| (v.id, v.ty.clone())));
        } else {
            unreachable!()
        }

        Ok(b)
    }

    fn enter_ast_match_branch(&mut self, b: AstMatchBranch) -> PResult<AstMatchBranch> {
        if let Some(scope) = &b.scope {
            self.variables
                .extend(scope.iter().map(|(_, v)| (v.id, v.ty.clone())));
        } else {
            unreachable!()
        }

        Ok(b)
    }

    fn enter_ast_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.satisfy_restrictions(&o.restrictions)?;

        // Nothing special here to do.
        Ok(o)
    }

    fn enter_ast_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.satisfy_restrictions(&o.restrictions)?;

        self.return_type.push(o.return_type.clone());
        self.variables = o
            .scope
            .as_ref()
            .unwrap()
            .iter()
            .map(|(_, v)| (v.id, v.ty.clone()))
            .collect();

        if let Some(block) = &o.definition {
            self.unify(true, &o.return_type, &block.ty)?;
        }

        Ok(o)
    }

    fn enter_ast_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.satisfy_restrictions(&t.restrictions)?;

        Ok(t)
    }

    fn enter_ast_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        self.satisfy_restrictions(&e.restrictions)?;

        Ok(e)
    }

    fn enter_ast_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        if let Some(trait_ty) = &i.trait_ty {
            self.satisfy_restrictions(&instantiate_trait_restrictions(
                &self.program,
                trait_ty,
                &i.impl_ty,
            )?)?;

            for (name, ty) in &i.associated_types {
                let restrictions: Vec<_> = instantiate_associated_ty_restrictions(
                    &self.program,
                    trait_ty,
                    &name,
                    &i.impl_ty,
                )?
                .into_iter()
                .map(|trt| AstTypeRestriction::new(ty.clone(), trt))
                .collect();

                self.satisfy_restrictions(&restrictions)?;
            }
        }

        Ok(i)
    }

    fn enter_ast_global_variable(&mut self, g: AstGlobalVariable) -> PResult<AstGlobalVariable> {
        self.unify(true, &g.ty, &g.init.ty)?;

        Ok(g)
    }

    fn exit_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.return_type.pop();
        self.variables.clear();

        Ok(f)
    }

    fn exit_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match self.normalize_ty(t)? {
            AstType::Object(name, generics) => {
                self.satisfy_well_formed_object(&name, &generics)?;
                Ok(AstType::Object(name, generics))
            },
            AstType::Enum(name, generics) => {
                self.satisfy_well_formed_enum(&name, &generics)?;
                Ok(AstType::Enum(name, generics))
            },
            t => Ok(t),
        }
    }

    fn exit_ast_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match &s {
            // Removed in earlier stages
            AstStatement::Expression { .. } => {},
            AstStatement::Let { pattern, value } => {
                self.unify_pattern(true, pattern, &value.ty)?;
            },
        }

        Ok(s)
    }

    fn exit_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { mut data, ty, span } = e;

        match &mut data {
            AstExpressionData::SelfRef
            | AstExpressionData::AllocateArray { .. }
            | AstExpressionData::ExprCall { .. }
            | AstExpressionData::ObjectCall { .. }
            | AstExpressionData::ArrayAccess { .. }
            | AstExpressionData::NamedEnum { .. }
            | AstExpressionData::PlainEnum { .. }
            | AstExpressionData::BinOp { .. }
            | AstExpressionData::Negate { .. }
            | AstExpressionData::Not { .. }
            | AstExpressionData::As { .. }
            | AstExpressionData::For { .. }
            | AstExpressionData::Throw { .. } => unreachable!(),

            AstExpressionData::Unimplemented => {},
            AstExpressionData::Block { block } => {
                self.unify(true, &block.expression.ty, &ty)?;
            },
            AstExpressionData::If {
                condition,
                block,
                else_block,
            } => {
                self.unify(true, &condition.ty, &AstType::Bool)?;
                self.unify(true, &ty, &block.expression.ty)?;
                self.unify(true, &block.expression.ty, &else_block.expression.ty)?;
            },
            AstExpressionData::Match {
                expression,
                branches,
            } => {
                let match_expr_ty = &expression.ty;

                for AstMatchBranch {
                    pattern,
                    expression,
                    ..
                } in branches
                {
                    self.unify_pattern(true, pattern, match_expr_ty)?;
                    self.unify(true, &expression.ty, &ty)?;
                }
            },

            AstExpressionData::While {
                id,
                condition,
                block,
                else_block,
                ..
            } => {
                self.unify(true, &condition.ty, &AstType::Bool)?;
                self.unify(true, &block.expression.ty, &AstType::none())?;
                self.unify(true, &else_block.expression.ty, &self.loops[id].clone())?;
            },

            AstExpressionData::Literal(lit) => match lit {
                AstLiteral::True | AstLiteral::False => {
                    self.unify(true, &ty, &AstType::Bool)?;
                },
                AstLiteral::String { .. } => self.unify(true, &ty, &AstType::String)?,
                AstLiteral::Int(..) => {
                    self.unify(true, &ty, &AstType::Int)?;
                },
                AstLiteral::Float(..) => {
                    self.unify(true, &ty, &AstType::Float)?;
                },
                AstLiteral::Char(..) => {
                    self.unify(true, &ty, &AstType::Char)?;
                },
            },
            AstExpressionData::Identifier { variable_id, .. } => {
                if !self.variables.contains_key(variable_id.as_ref().unwrap()) {
                    return perror_at!(
                        span,
                        "Cannot find {:?} in {:?}",
                        variable_id,
                        self.variables
                    );
                }

                let variable_ty = self.variables[variable_id.as_ref().unwrap()].clone();
                self.unify(true, &ty, &variable_ty)?;
            },
            AstExpressionData::GlobalVariable { name } => {
                self.unify(true, &ty, &self.program.clone().analyzed_globals[name])?;
            },
            AstExpressionData::Tuple { values } => {
                let tuple_tys = into_types(values);
                self.unify(true, &ty, &AstType::tuple(tuple_tys))?;
            },
            AstExpressionData::ArrayLiteral { elements } => {
                let _tuple_tys = into_types(elements);
                let elem_ty = AstType::infer();

                for elem in elements {
                    self.unify(true, &elem.ty, &elem_ty)?;
                }

                self.unify(true, &AstType::array(elem_ty), &ty)?;
            },

            // A regular function call
            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => {
                let (param_tys, return_ty, objectives) =
                    instantiate_fn_signature(&self.program, fn_name, generics)?;
                let arg_tys = into_types(args);
                self.unify_all(true, &param_tys, &arg_tys)?;
                self.unify(true, &return_ty, &ty)?;
                self.satisfy_restrictions(&objectives)?; // Add fn restrictions
            },
            // Call an object's static function
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                impl_signature,
            } => {
                // If neither is specified, then we should try looking for an impl.
                if associated_trait.is_none() && impl_signature.is_none() {
                    if let Some((trait_candidate, impl_candidate)) = self.satisfy_method(
                        call_type,
                        fn_name,
                        fn_generics,
                        &into_types(&args),
                        &ty,
                    )? {
                        *associated_trait = trait_candidate;
                        *impl_signature = Some(impl_candidate);
                    }
                }

                if let Some(associated_trait) = associated_trait {
                    let fn_data =
                        &self.program.analyzed_traits[&associated_trait.trt.name].methods[fn_name];

                    let expected_args = fn_data.parameters.len();
                    if args.len() != expected_args {
                        return perror_at!(
                            span,
                            "Incorrect number of arguments for method `<{} as {}>:{}(...)`. \
                             Expected {}, found {}.",
                            call_type,
                            associated_trait,
                            fn_name,
                            expected_args,
                            args.len()
                        );
                    }

                    let expected_generics = fn_data.generics.len();
                    if fn_generics.len() == expected_generics {
                        /* Don't do anything. */
                    } else if fn_generics.len() == 0 {
                        *fn_generics = (0..expected_generics).map(|_| AstType::infer()).collect();
                    } else {
                        return perror_at!(
                            span,
                            "Incorrect number of generics for symbol `<{} as {}>:{}(...)`. \
                             Expected {}, found {}.",
                            call_type,
                            associated_trait,
                            fn_name,
                            expected_generics,
                            fn_generics.len()
                        );
                    };

                    let (param_tys, return_ty, objectives) = instantiate_trait_fn_signature(
                        &*self.program,
                        &associated_trait.trt.name,
                        &associated_trait.trt.generics,
                        fn_name,
                        fn_generics,
                        &call_type,
                    )?;
                    let arg_tys = into_types(args);
                    self.unify_all(true, &param_tys, &arg_tys)?;
                    self.unify(true, &return_ty, &ty)?;
                    self.satisfy_restrictions(&objectives)?;

                    if impl_signature.is_none() {
                        debug!(
                            "{}Satisfying impl for {}",
                            INDENT.repeat(self.epochs.len()),
                            self.normalize_ty(call_type.clone())?
                        );

                        let impl_candidate = self.satisfy_method_with_trait(
                            call_type,
                            associated_trait,
                            fn_name,
                            fn_generics,
                            &into_types(&args),
                            &ty,
                        )?;
                        *impl_signature = impl_candidate;
                    }
                }

                if let Some(impl_signature) = impl_signature {
                    let impl_data = &self.program.analyzed_impls[&impl_signature.impl_id];

                    if impl_data.is_regular() {
                        let fn_data = &impl_data.methods[fn_name];

                        let expected_args = fn_data.parameters.len();
                        if args.len() != expected_args {
                            return perror_at!(
                                span,
                                "Incorrect number of arguments for method `{}:{}(...)`. Expected \
                                 {}, found {}.",
                                call_type,
                                fn_name,
                                expected_args,
                                args.len()
                            );
                        }

                        let expected_generics = fn_data.generics.len();
                        if fn_generics.len() == expected_generics {
                            /* Don't do anything. */
                        } else if fn_generics.len() == 0 {
                            *fn_generics =
                                (0..expected_generics).map(|_| AstType::infer()).collect();
                        } else {
                            return perror_at!(
                                span,
                                "Incorrect number of generics for symbol `<{}>:{}(...)`. Expected \
                                 {}, found {}.",
                                call_type,
                                fn_name,
                                expected_generics,
                                fn_generics.len()
                            );
                        };

                        let (param_tys, return_ty, objectives) = instantiate_impl_fn_signature(
                            &*self.program,
                            impl_signature.impl_id,
                            &impl_signature.generics,
                            fn_name,
                            fn_generics,
                            &call_type,
                        )?;

                        let arg_tys = into_types(args);
                        self.unify_all(true, &param_tys, &arg_tys)?;
                        self.unify(true, &return_ty, &ty)?;
                        self.satisfy_restrictions(&objectives)?;
                    }
                }
            },
            // A tuple access `a:1`
            AstExpressionData::TupleAccess { accessible, idx } => {
                let tuple_ty = &accessible.ty;

                match tuple_ty {
                    AstType::Tuple { types } => {
                        if types.len() <= *idx {
                            return perror_at!(
                                span,
                                "Cannot access tuple `{}` at index {}",
                                tuple_ty,
                                idx,
                            );
                        }

                        self.unify(true, &ty, &types[*idx])?;
                    },
                    AstType::AssociatedType { .. } | AstType::Infer(_) => {
                        if top_epoch_mut!(self).ambiguous.is_none() {
                            top_epoch_mut!(self).ambiguous =
                                Some(format!("Ambiguous tuple access {}:{}", tuple_ty, idx));
                        }
                    },
                    t => {
                        return perror_at!(span, "Cannot perform tuple access on type `{}`", t,);
                    },
                }
            },
            // Call an object's member
            AstExpressionData::ObjectAccess {
                object,
                mem_name,
                mem_idx,
            } => {
                let object_ty = &object.ty;

                match object_ty {
                    AstType::Object(obj_name, generics) => {
                        if !self.program.analyzed_objects[obj_name]
                            .member_tys
                            .contains_key(mem_name)
                        {
                            return perror_at!(
                                span,
                                "Cannot access object `{}` at member `{}`",
                                object_ty,
                                mem_name,
                            );
                        }

                        let member_ty =
                            instantiate_object_member(&self.program, obj_name, generics, mem_name)?;
                        self.unify(true, &ty, &member_ty)?;

                        *mem_idx = Some(
                            self.program.analyzed_objects[&*obj_name].member_indices[&*mem_name],
                        );
                    },
                    AstType::AssociatedType { .. } | AstType::Infer(_) => {
                        if top_epoch_mut!(self).ambiguous.is_none() {
                            top_epoch_mut!(self).ambiguous = Some(format!(
                                "Ambiguous object access {}:{}",
                                object_ty, mem_name
                            ));
                        }
                    },
                    t => {
                        return perror_at!(span, "Cannot perform object access on type `{}`", t,);
                    },
                }
            },

            AstExpressionData::AllocateObject {
                object,
                generics,
                children,
                ..
            } => {
                let expected_tys = instantiate_object_members(&self.program, object, &generics)?;

                for (child, expr) in children {
                    self.unify(true, &expected_tys[child], &expr.ty)?;
                }

                self.unify(
                    true,
                    &AstType::Object(object.clone(), generics.clone()),
                    &ty,
                )?;
            },

            AstExpressionData::Assign { lhs, rhs } => {
                let lhs_ty = &lhs.ty;
                let rhs_ty = &rhs.ty;
                self.unify(true, lhs_ty, rhs_ty)?;
                self.unify(true, lhs_ty, &ty)?;
            },

            AstExpressionData::GlobalFn { name, generics } => {
                let (params, ret_ty, restrictions) =
                    instantiate_fn_signature(&self.program, name, generics)?;
                let fn_ptr_ty = AstType::fn_ptr_type(params, ret_ty);
                self.unify(true, &ty, &fn_ptr_ty)?;
                self.satisfy_restrictions(&restrictions)?;
            },

            AstExpressionData::Closure { params, expr, .. } => {
                let return_ty = self.return_type.pop().unwrap();
                let param_tys = params.iter().map(|param| param.ty.clone()).collect();

                self.unify(true, &return_ty, &expr.ty)?;
                self.unify(true, &ty, &AstType::closure_type(param_tys, return_ty))?;
            },
            AstExpressionData::Async { expr, .. } => {
                let return_ty = self.return_type.pop().unwrap();
                let awaitable_name = self
                    .program
                    .construct_obj_ref("std::asynchronous::Awaitable")?;

                self.unify(true, &return_ty, &expr.ty)?;
                self.unify(true, &ty, &AstType::object(awaitable_name, vec![return_ty]))?;
            },

            AstExpressionData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
            } => {
                self.unify(
                    true,
                    &ty,
                    &AstType::enumerable(enumerable.clone(), generics.clone()),
                )?;

                let expected_tys =
                    instantiate_enum_pattern(&self.program, enumerable, &generics, &variant)?;

                for (child, ty) in
                    ZipExact::zip_exact(children, expected_tys, "positional elements")?
                {
                    self.unify(true, &child.ty, &ty)?;
                }
            },

            AstExpressionData::Instruction {
                output: InstructionOutput::Anonymous(_),
                ..
            } => {
                self.unify(true, &ty, &AstType::none())?;
            },

            AstExpressionData::Instruction {
                output: InstructionOutput::Type(t),
                ..
            } => {
                self.unify(true, &ty, &t)?;
            },

            AstExpressionData::Continue { .. } => {
                self.unify(true, &ty, &AstType::none())?;
            },

            AstExpressionData::Break { id, value, .. } => {
                self.unify(true, &ty, &AstType::none())?;
                let id = id.as_ref().unwrap();
                self.unify(true, &self.loops[id].clone(), &value.ty)?;
            },

            AstExpressionData::Return { value } => {
                self.unify(true, &ty, &AstType::none())?;

                let return_ty = self
                    .return_type
                    .last()
                    .ok_or_else(|| {
                        PError::new_at(span, format!("Unexpected `return` in non-function context"))
                    })?
                    .clone();

                self.unify(true, &value.ty, &return_ty)?;
            },
            AstExpressionData::Await {
                value,
                impl_signature,
                associated_trait,
            } => {
                let pollable_ty = value.ty.clone();
                let poll_result_ty = ty.clone();

                if associated_trait.is_none() {
                    *associated_trait = Some(AstTraitTypeWithAssocs::new(
                        self.program.construct_trt_ref("std::asynchronous::Poll")?,
                        vec![],
                        btreemap! { "Result".to_string() => poll_result_ty.clone() },
                    ));
                }

                if impl_signature.is_none() {
                    debug!(
                        "{}Satisfying impl for {}",
                        INDENT.repeat(self.epochs.len()),
                        self.normalize_ty(pollable_ty.clone())?
                    );

                    let impl_candidate = self.satisfy_method_with_trait(
                        &pollable_ty,
                        associated_trait.as_ref().unwrap(),
                        "poll",
                        &[],
                        &[pollable_ty.clone()],
                        &AstType::tuple(vec![
                            AstType::enumerable(
                                self.program
                                    .construct_enum_ref("std::asynchronous::PollState")?,
                                vec![poll_result_ty],
                            ),
                            pollable_ty.clone(),
                        ]),
                    )?;

                    *impl_signature = impl_candidate;
                }
            },

            AstExpressionData::Assert { condition } => {
                self.unify(true, &ty, &AstType::none())?;
                self.unify(true, &condition.ty, &AstType::Bool)?;
            },

            AstExpressionData::ConditionalCompilation { branches } =>
                for (_, branch) in branches {
                    self.unify(true, &ty, &branch.expression.ty)?;
                },
        }

        Ok(AstExpression { data, ty, span })
    }

    fn exit_ast_match_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        self.unify_pattern(true, &p, &p.ty)?;

        Ok(p)
    }

    fn exit_ast_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.return_type.pop();
        self.variables.clear();

        Ok(o)
    }
}

impl TyckAdapter for TyckSolver {
    fn exit_tyck_instantiated_object_function(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        let TyckInstantiatedObjectFunction {
            fun,
            impl_ty,
            trait_ty,
            fn_generics,
        } = &i;

        if let Some(trait_ty) = trait_ty {
            let (expected_params, expected_ret_ty, expected_constraints) =
                instantiate_trait_fn_signature(
                    &self.program,
                    &trait_ty.name,
                    &trait_ty.generics,
                    &fun.name,
                    &fn_generics,
                    &impl_ty,
                )?;

            for (given_param, expected_ty) in
                ZipExact::zip_exact(&fun.parameter_list, &expected_params, "parameter")?
            {
                self.unify(true, expected_ty, &given_param.ty)?;
            }

            self.unify(true, &expected_ret_ty, &fun.return_type)?;
            self.satisfy_restrictions(&expected_constraints)?;

            if top_epoch!(self).ambiguous.is_none() {
                let given_constraints: HashSet<_> = fun.restrictions.iter().cloned().collect();

                // Normalize our expected constraints
                let expected_constraints: HashSet<_> =
                    expected_constraints.visit(self)?.into_iter().collect();

                for c in &given_constraints {
                    if !expected_constraints.contains(c) {
                        top_epoch_mut!(self).ambiguous = Some(format!(
                            "The impl method has an additional constraint not specified in the \
                             trait prototype: \n{:?}\n{:?}",
                            expected_constraints, given_constraints
                        ));
                        break;
                    }
                }

                /* NOTE: I don't particularly care if the constraints are subset here... I THINK.

                for c in &expected_constraints {
                    if !given_constraints.contains(c) {
                        return perror!(
                            "The impl method has an additional \
                        constraint not specified in the trait prototype"
                        );
                    }
                } */
            }
        }

        Ok(i)
    }

    fn exit_tyck_instantiated_impl(
        &mut self,
        i: TyckInstantiatedImpl,
    ) -> PResult<TyckInstantiatedImpl> {
        let TyckInstantiatedImpl {
            impl_ty,
            trait_ty,
            impl_signature: _,
        } = i;

        let trait_ty = AstTraitTypeWithAssocs {
            trt: trait_ty,
            assoc_bindings: btreemap! {},
        };

        // TODO: Can we stop doing this once we've got the signature memoized?
        let impl_signature = self.satisfy_impl(&impl_ty, &trait_ty)?;

        Ok(TyckInstantiatedImpl {
            impl_ty,
            trait_ty: trait_ty.trt,
            impl_signature,
        })
    }
}

impl AnAdapter for TyckSolver {
    fn enter_an_impl_data(&mut self, i: AnImplData) -> PResult<AnImplData> {
        assert!(i.is_dummy());

        if let Some(trait_ty) = &i.trait_ty {
            self.satisfy_restrictions(&instantiate_trait_restrictions(
                &self.program,
                trait_ty,
                &i.impl_ty,
            )?)?;

            for (name, ty) in &i.associated_tys {
                let restrictions: Vec<_> = instantiate_associated_ty_restrictions(
                    &self.program,
                    trait_ty,
                    &name,
                    &i.impl_ty,
                )?
                .into_iter()
                .map(|trt| AstTypeRestriction::new(ty.clone(), trt))
                .collect();

                self.satisfy_restrictions(&restrictions)?;
            }
        }

        Ok(i)
    }
}

fn into_types(values: &[AstExpression]) -> Vec<AstType> {
    values.iter().map(|e| e.ty.clone()).collect()
}

struct NormalizationAdapter<'a>(&'a mut TyckSolver);

impl<'a> AstAdapter for NormalizationAdapter<'a> {
    fn exit_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Infer(id) =>
                if let Some(t) = top_epoch!(self.0).inferences.get(&id) {
                    trace!("Normalized _{} as {}", id.0, t);
                    t.clone().visit(self)
                } else {
                    Ok(AstType::Infer(id))
                },
            AstType::AssociatedType {
                obj_ty,
                trait_ty: None,
                name,
            } =>
                if let Some(imp) = self.0.satisfy_associated_type(&obj_ty, &name)? {
                    instantiate_associated_ty(
                        &self.0.program,
                        imp.impl_id,
                        &imp.generics,
                        &name,
                        &obj_ty,
                    )?
                    .visit(self)
                } else {
                    Ok(AstType::AssociatedType {
                        obj_ty,
                        trait_ty: None,
                        name,
                    })
                },
            AstType::AssociatedType {
                obj_ty,
                trait_ty: Some(trait_ty),
                name,
            } =>
                if let Some(imp) = self.0.satisfy_impl(&obj_ty, &trait_ty)? {
                    instantiate_associated_ty(
                        &self.0.program,
                        imp.impl_id,
                        &imp.generics,
                        &name,
                        &obj_ty,
                    )?
                    .visit(self)
                } else {
                    Ok(AstType::AssociatedType {
                        obj_ty,
                        trait_ty: Some(trait_ty),
                        name,
                    })
                },
            t => Ok(t),
        }
    }
}

pub struct TypeAmbiguityAdapter<'a>(&'a mut TyckSolver);

impl<'a> AstAdapter for TypeAmbiguityAdapter<'a> {
    fn exit_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::Infer(_) = &t {
            if top_epoch!(self.0).ambiguous.is_none() {
                top_epoch_mut!(self.0).ambiguous = Some(format!("Ambiguous infer type `{}`", t));
            }
        }

        Ok(t)
    }
}

impl<'a> AnAdapter for TypeAmbiguityAdapter<'a> {}
impl<'a> TyckAdapter for TypeAmbiguityAdapter<'a> {}
