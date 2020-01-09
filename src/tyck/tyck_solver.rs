use crate::ana::represent::{AnImplData, AnalyzedProgram};
use crate::ana::represent_visitor::AnAdapter;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::tyck::tyck_instantiation::*;
use crate::tyck::{TyckAdapter, TyckInstantiatedObjectFunction, TYCK_MAX_DEPTH};
use crate::util::{IntoError, PResult, Visit, ZipExact};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TyckSolver {
    analyzed_program: Rc<AnalyzedProgram>,
    epochs: Vec<TyckEpoch>,

    // Temporary values
    return_type: Option<AstType>,
    variables: HashMap<VariableId, AstType>,
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
    ( $x:expr ) => {
        $x.epochs.last().unwrap()
    };
}

macro_rules! top_epoch_mut {
    ( $x:expr ) => {
        $x.epochs.last_mut().unwrap()
    };
}

const INDENT: &'static str = "   ";

impl TyckSolver {
    pub fn new(analyzed_program: Rc<AnalyzedProgram>) -> TyckSolver {
        TyckSolver {
            analyzed_program,
            epochs: vec![TyckEpoch {
                inferences: HashMap::new(),
                successes: HashMap::new(),
                ambiguous: None,
            }],
            return_type: None,
            variables: HashMap::new(),
        }
    }

    pub fn typecheck_loop<T>(&mut self, t: T) -> PResult<T>
    where
        T: for<'a> Visit<TypeAmbiguityAdapter<'a>> + Visit<TyckSolver> + Clone + Eq + Debug,
    {
        println!("\n\n\n===== ===== Start ze loop! ===== =====");
        let mut t = t;

        for i in 0.. {
            println!("\n===== LOOP ({}) =====", i);

            let new_t = t
                .clone()
                .visit(self)?
                .visit(&mut TypeAmbiguityAdapter(self))?;
            let epoch = top_epoch_mut!(self);

            if new_t == t {
                if let Some(ambiguity) = &epoch.ambiguous {
                    println!("Yikes:");

                    for (x, y) in &epoch.inferences {
                        println!("_{} => {}", x.0, y);
                    }

                    println!("{:#?}", t);
                    return PResult::error(format!("Tyck: {}", ambiguity));
                } else {
                    return Ok(t);
                }
            }

            t = new_t;
            epoch.ambiguous = None;
        }

        unreachable!()
    }

    fn unify(&mut self, full_unify: bool, a: &AstType, b: &AstType) -> PResult<()> {
        println!(
            "{}Unifying {} and {} (full? {})",
            INDENT.repeat(self.epochs.len()),
            a,
            b,
            full_unify
        );

        let (a, b) = (self.normalize_ty(a.clone())?, self.normalize_ty(b.clone())?);

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
            }

            (AstType::Infer(lid), AstType::Infer(rid)) if lid == rid => {
                /* Do nothing. No cycles in this house. */
            }
            (AstType::Infer(id), ty) => {
                if top_epoch_mut!(self)
                    .inferences
                    .insert(id, ty.clone())
                    .is_some()
                {
                    panic!("ICE: Duplicated inference, normalization should not let this happen!");
                }
            }

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
                        panic!(
                            "ICE: Duplicated inference, normalization should not let this happen!"
                        );
                    }
                }
            }

            (AstType::Int, AstType::Int)
            | (AstType::Char, AstType::Char)
            | (AstType::Bool, AstType::Bool)
            | (AstType::String, AstType::String) => {}

            (AstType::DummyGeneric(a, ..), AstType::DummyGeneric(b, ..)) if a == b => {}
            (AstType::Dummy(a), AstType::Dummy(b)) if a == b => {}

            (AstType::Array { ty: a_ty }, AstType::Array { ty: b_ty }) => {
                self.unify(full_unify, &a_ty, &b_ty)?;
            }

            (AstType::Tuple { types: a_tys }, AstType::Tuple { types: b_tys }) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "tuple types")? {
                    self.unify(full_unify, &a_ty, &b_ty)?;
                }
            }

            (AstType::Object(a_name, a_tys), AstType::Object(b_name, b_tys)) => {
                if a_name != b_name {
                    return PResult::error(format!(
                        "Cannot equate distinct objects: {} and {}",
                        a_name.full_name()?,
                        b_name.full_name()?
                    ));
                } else {
                    for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "object generics")? {
                        self.unify(full_unify, &a_ty, &b_ty)?;
                    }
                }
            }

            (AstType::Enum(a_name, a_tys), AstType::Enum(b_name, b_tys)) => {
                if a_name != b_name {
                    return PResult::error(format!(
                        "Cannot equate distinct enums: {} and {}",
                        a_name.full_name()?,
                        b_name.full_name()?
                    ));
                } else {
                    for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "enum generics")? {
                        self.unify(full_unify, &a_ty, &b_ty)?;
                    }
                }
            }

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
            }

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
            }

            (a, b) => {
                if &format!("Type non-union, {} and {}", a, b) == "Type non-union, () and _T101(dg)"
                {
                    panic!("Right here, baby!");
                }

                return PResult::error(format!("Type non-union, {} and {}", a, b));
            }
        }

        Ok(())
    }

    fn unify_all(&mut self, full_unify: bool, a: &[AstType], b: &[AstType]) -> PResult<()> {
        assert_eq!(a.len(), b.len());

        for (a, b) in Iterator::zip(a.iter(), b.iter()) {
            self.unify(full_unify, a, b)?;
        }

        Ok(())
    }

    fn full_unify_pattern(&mut self, pattern: &AstMatchPattern, other_ty: &AstType) -> PResult<()> {
        self.unify(true, &pattern.ty, other_ty)?;

        match &pattern.data {
            AstMatchPatternData::Underscore => {}
            AstMatchPatternData::Identifier(v) => self.unify(true, &v.ty, other_ty)?,
            AstMatchPatternData::Tuple(children) => {
                let mut children_tys = Vec::new();

                for child in children {
                    let child_ty = AstType::infer();
                    self.full_unify_pattern(child, &child_ty)?;
                    children_tys.push(child_ty);
                }

                self.unify(true, &AstType::tuple(children_tys), other_ty)?;
            }
            AstMatchPatternData::Literal(lit) => match lit {
                AstLiteral::True | AstLiteral::False => {
                    self.unify(true, &AstType::Bool, other_ty)?
                }
                AstLiteral::Int(..) => self.unify(true, &AstType::Int, other_ty)?,
                AstLiteral::Char(..) => self.unify(true, &AstType::Char, other_ty)?,
                AstLiteral::String { .. } => self.unify(true, &AstType::String, other_ty)?,
            },
            AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ..
            } => {
                let expected_tys = instantiate_enum_pattern(
                    &self.analyzed_program,
                    enumerable,
                    &generics,
                    &variant,
                )?;

                for (child, ty) in
                    ZipExact::zip_exact(children, expected_tys, "positional elements")?
                {
                    self.full_unify_pattern(child, &ty)?;
                }

                self.unify(
                    true,
                    &other_ty,
                    &AstType::enumerable(enumerable.clone(), generics.clone()),
                )?;
            }
            AstMatchPatternData::PlainEnum { .. } | AstMatchPatternData::NamedEnum { .. } => {
                unreachable!()
            }
        }

        Ok(())
    }

    fn commit_epoch(&mut self) -> PResult<()> {
        let inferences = top_epoch!(self).inferences.clone().visit(self)?;

        let mut successes = HashMap::new();
        for (obj, imp) in top_epoch!(self).successes.clone() {
            let obj = match obj {
                TyckObjective::Impl(ty, trt) => {
                    TyckObjective::Impl(ty.visit(self)?, trt.visit(self)?)
                }
                TyckObjective::Method(ty, name) => TyckObjective::Method(ty.visit(self)?, name),
                TyckObjective::AssociatedType(ty, name) => {
                    TyckObjective::AssociatedType(ty.visit(self)?, name)
                }
                TyckObjective::WellFormed(name, tys) => {
                    TyckObjective::WellFormed(name, tys.visit(self)?)
                }
            };

            let imp = imp.visit(self)?;

            if let Some(other_imp) = successes.get(&obj) {
                if imp != *other_imp {
                    return PResult::error(format!("Conflict impls.... TODO: better message",));
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
            panic!("ICE: Cannot roll back when there are no epochs on the stack!")
        })
    }

    fn push_new_epoch(&mut self) -> PResult<()> {
        if self.epochs.len() > TYCK_MAX_DEPTH {
            panic!("ICE: OVERFLOW");
        }

        let top = top_epoch!(self);
        let new = TyckEpoch {
            inferences: top.inferences.clone(),
            successes: top.successes.clone(),
            ambiguous: None,
        };

        self.epochs.push(new);
        Ok(())
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

        println!(
            "{}Satisfying {} :- {}",
            INDENT.repeat(self.epochs.len()),
            ty,
            trt
        );

        let trait_data = &self.analyzed_program.clone().analyzed_traits[&trt.trt.name];
        let mut solutions = Vec::new();

        for imp in &trait_data.impls {
            let impl_generics: Vec<_> = self.analyzed_program.analyzed_impls[imp]
                .generics
                .iter()
                .map(|_| AstType::infer())
                .collect();

            self.push_new_epoch()?;
            let sol = self.elaborate_impl(*imp, &impl_generics, ty, &trt.trt);
            let epoch = self.rollback_epoch();

            print!("{}", INDENT.repeat(self.epochs.len()));
            if let Err(e) = &sol {
                println!("... Err: {}", e.why());
            } else if let Some(a) = &epoch.ambiguous {
                println!("... Amb: {}", a);
            } else {
                println!("... Ok!");
            }

            solutions.push((sol, epoch));
        }

        match self.internal_disambiguate(solutions) {
            Ok(Some(imp)) => {
                let (a, b, c) = instantiate_impl_signature(
                    &self.analyzed_program,
                    imp.impl_id,
                    &imp.generics,
                    ty,
                )?;
                println!(
                    "{}>Ok impl {} for {} where {:?}",
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
                            &self.analyzed_program,
                            imp.impl_id,
                            &imp.generics,
                            name,
                            ty,
                        )?,
                        bound_ty,
                    )?;
                }

                // NOTE: we don't key this objective, since it gets memoized in the internal fn.
                Ok(Some(imp))
            }
            Ok(None) => {
                println!("{}>Ambig", INDENT.repeat(self.epochs.len()),);
                if top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous =
                        Some(format!("Ambiguous impl `{}` for `{}`", trt, ty));
                }
                Ok(None)
            }
            Err(_) => {
                println!("{}>Err", INDENT.repeat(self.epochs.len()),);
                PResult::error(format!(
                    "No suitable solution for impl `{}` for `{}`",
                    trt, ty
                ))
            }
        }
    }

    fn elaborate_impl(
        &mut self,
        impl_id: ImplId,
        impl_generics: &[AstType],
        ty: &AstType,
        trt: &AstTraitType,
    ) -> PResult<AstImplSignature> {
        let impl_info = &self.analyzed_program.clone().analyzed_impls[&impl_id];
        println!(
            "{}Trying impl {} for {} where {:?}",
            INDENT.repeat(self.epochs.len()),
            impl_info.trait_ty,
            impl_info.impl_ty,
            impl_info.restrictions
        );

        let (expected_ty, expected_trt, restrictions) =
            instantiate_impl_signature(&self.analyzed_program, impl_id, impl_generics, ty)?;

        self.unify(false, &expected_ty, ty)?;
        self.unify_all(false, &expected_trt.generics, &trt.generics)?;

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

        println!(
            "{}Satisfying {} assoc ty {}",
            INDENT.repeat(self.epochs.len()),
            ty,
            name
        );

        let mut solutions = Vec::new();
        for trt in &self.analyzed_program.clone().methods_to_traits[name] {
            let trait_data = &self.analyzed_program.clone().analyzed_traits[&trt];

            for imp in &trait_data.impls {
                let impl_generics: Vec<_> = self.analyzed_program.analyzed_impls[imp]
                    .generics
                    .iter()
                    .map(|_| AstType::infer())
                    .collect();

                self.push_new_epoch()?;
                let sol = self.elaborate_impl(
                    *imp,
                    &impl_generics,
                    ty,
                    &instantiate_impl_trait_ty(&self.analyzed_program, *imp, &impl_generics, ty)?
                        .trt,
                );
                let epoch = self.rollback_epoch();

                print!("{}", INDENT.repeat(self.epochs.len()));
                if let Err(e) = &sol {
                    println!("... Err: {}", e.why());
                } else if let Some(a) = &epoch.ambiguous {
                    println!("... Amb: {}", a);
                } else {
                    println!("... Ok!");
                }

                solutions.push((sol, epoch));
            }
        }

        match self.internal_disambiguate(solutions) {
            Ok(Some(imp)) => {
                let (a, b, c) = instantiate_impl_signature(
                    &self.analyzed_program,
                    imp.impl_id,
                    &imp.generics,
                    ty,
                )?;
                println!(
                    "{}>Ok assoc = impl {} for {} where {:?}",
                    INDENT.repeat(self.epochs.len()),
                    b,
                    a,
                    c
                );
                top_epoch_mut!(self)
                    .successes
                    .insert(key, Some(imp.clone()));
                Ok(Some(imp))
            }
            Ok(None) => {
                println!("{}>Ambig assoc", INDENT.repeat(self.epochs.len()),);
                if top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous =
                        Some(format!("Ambiguous associated type `<{}>::{}`", ty, name));
                }
                Ok(None)
            }
            Err(_) => {
                println!("{}>Err assoc", INDENT.repeat(self.epochs.len()),);
                PResult::error(format!(
                    "No suitable trait for associated type `<{}>::{}`",
                    ty, name
                ))
            }
        }
    }

    fn satisfy_method(
        &mut self,
        ty: &AstType,
        name: &str,
        generics: &[AstType],
        arg_tys: &[AstType],
        return_ty: &AstType,
    ) -> PResult<Option<(AstTraitTypeWithAssocs, AstImplSignature)>> {
        let key = TyckObjective::Method(ty.clone(), name.to_owned());

        if let Some(Some(imp)) = top_epoch!(self).successes.get(&key) {
            return Ok(Some((
                instantiate_impl_trait_ty(&self.analyzed_program, imp.impl_id, &imp.generics, ty)?,
                imp.clone(),
            )));
        }

        println!(
            "{}Satisfying {} method {}",
            INDENT.repeat(self.epochs.len()),
            ty,
            name
        );

        let mut solutions = Vec::new();
        for trt in &self.analyzed_program.clone().methods_to_traits[name] {
            let trait_data = &self.analyzed_program.clone().analyzed_traits[&trt];
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
                let impl_generics: Vec<_> = self.analyzed_program.analyzed_impls[imp]
                    .generics
                    .iter()
                    .map(|_| AstType::infer())
                    .collect();

                self.push_new_epoch()?;
                let sol = self.elaborate_method(
                    *imp,
                    &impl_generics,
                    name,
                    &fn_generics,
                    arg_tys,
                    return_ty,
                    ty,
                );
                let epoch = self.rollback_epoch();

                print!("{}", INDENT.repeat(self.epochs.len()));
                if let Err(e) = &sol {
                    println!("... Err: {}", e.why());
                } else if let Some(a) = &epoch.ambiguous {
                    println!("... Amb: {}", a);
                } else {
                    println!("... Ok!");
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
                    instantiate_impl_trait_ty(
                        &self.analyzed_program,
                        imp.impl_id,
                        &imp.generics,
                        ty,
                    )?,
                    imp,
                )))
            }
            Ok(None) => {
                if top_epoch!(self).ambiguous.is_none() {
                    top_epoch_mut!(self).ambiguous =
                        Some(format!("Ambiguous method `<{}>:{}(...)`", ty, name));
                }
                Ok(None)
            }
            Err(_) => PResult::error(format!(
                "No suitable trait for method `<{}>:{}(...)`",
                ty, name
            )),
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
    ) -> PResult<AstImplSignature> {
        let impl_info = &self.analyzed_program.clone().analyzed_impls[&impl_id];
        println!(
            "{}Trying impl {} for {} where {:?}",
            INDENT.repeat(self.epochs.len()),
            impl_info.trait_ty,
            impl_info.impl_ty,
            impl_info.restrictions
        );

        let (expected_call_ty, _, restrictions) =
            instantiate_impl_signature(&self.analyzed_program, impl_id, impl_generics, call_ty)?;
        self.unify(false, &expected_call_ty, call_ty)?;

        if !impl_info.is_dummy {
            let (expected_args, expected_return_ty, _) = instantiate_impl_fn_signature(
                &self.analyzed_program,
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
                    return Ok(None);
                }

                if let Some((older_signature, _)) = &solution {
                    if self.analyzed_program.analyzed_impls[&older_signature.impl_id].is_dummy {
                        continue;
                    }

                    if !self.analyzed_program.analyzed_impls[&signature.impl_id].is_dummy {
                        return PResult::error(format!("Conflicting solutions!"));
                    }
                }

                solution = Some((signature, epoch));
            }
        }

        if let Some((signature, epoch)) = solution {
            self.push_epoch(epoch);
            self.commit_epoch()?;

            Ok(Some(signature))
        } else {
            PResult::error(format!("No solutions!"))
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

        println!(
            "{}Satisfying {} is well formed",
            INDENT.repeat(self.epochs.len()),
            AstType::Object(name.clone(), generics.to_vec())
        );

        if self.satisfy_restrictions(&instantiate_object_restrictions(
            &self.analyzed_program,
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

        println!(
            "{}Satisfying {} is well formed",
            INDENT.repeat(self.epochs.len()),
            AstType::Enum(name.clone(), generics.to_vec())
        );

        if self.satisfy_restrictions(&instantiate_enum_restrictions(
            &self.analyzed_program,
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
        let mut ret_tys = Vec::new();

        for t in tys {
            ret_tys.push(self.normalize_ty(t.clone())?);
        }

        Ok(ret_tys)
    }
}

impl AstAdapter for TyckSolver {
    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.satisfy_restrictions(&instantiate_trait_restrictions(
            &self.analyzed_program,
            &i.trait_ty,
            &i.impl_ty,
        )?)?;

        for (name, ty) in &i.associated_types {
            let restrictions: Vec<_> = instantiate_associated_ty_restrictions(
                &self.analyzed_program,
                &i.trait_ty,
                &name,
                &ty,
            )?
            .into_iter()
            .map(|trt| AstTypeRestriction::new(ty.clone(), trt))
            .collect();

            self.satisfy_restrictions(&restrictions)?;
        }

        Ok(i)
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.satisfy_restrictions(&t.restrictions)?;

        Ok(t)
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.satisfy_restrictions(&o.restrictions)?;

        // Nothing special here to do.
        Ok(o)
    }

    fn enter_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        self.satisfy_restrictions(&e.restrictions)?;

        Ok(e)
    }

    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.satisfy_restrictions(&f.restrictions)?;

        self.return_type = Some(f.return_type.clone());
        self.variables = f
            .variables
            .iter()
            .map(|(&k, v)| (k, v.ty.clone()))
            .collect();

        if let Some(block) = &f.definition {
            self.unify(true, &f.return_type, &block.expression.ty)?;
        }

        Ok(f)
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.satisfy_restrictions(&o.restrictions)?;

        self.return_type = Some(o.return_type.clone());
        self.variables = o
            .variables
            .iter()
            .map(|(&k, v)| (k, v.ty.clone()))
            .collect();

        if let Some(block) = &o.definition {
            self.unify(true, &o.return_type, &block.expression.ty)?;
        }

        Ok(o)
    }

    fn exit_global_variable(&mut self, g: AstGlobalVariable) -> PResult<AstGlobalVariable> {
        self.unify(true, &g.ty, &g.init.ty)?;

        Ok(g)
    }

    fn exit_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match &s {
            // Removed in earlier stages
            AstStatement::For { .. } => unreachable!(),

            AstStatement::Expression { .. } | AstStatement::Break | AstStatement::Continue => {}
            AstStatement::Let { pattern, value } => {
                self.full_unify_pattern(pattern, &value.ty)?;
            }
            AstStatement::While { condition, .. } => {
                self.unify(true, &condition.ty, &AstType::Bool)?;
            }
            AstStatement::Return { value } => {
                let return_ty = self.return_type.clone().unwrap();
                self.unify(true, &value.ty, &return_ty)?;
            }
            AstStatement::Assert { condition } => {
                self.unify(true, &condition.ty, &AstType::Bool)?;
            }
        }

        Ok(s)
    }

    fn exit_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { mut data, ty, span } = e;

        match &mut data {
            AstExpressionData::Unimplemented => {}
            AstExpressionData::Block { block } => {
                self.unify(true, &block.expression.ty, &ty)?;
            }
            AstExpressionData::If {
                condition,
                block,
                else_block,
            } => {
                self.unify(true, &condition.ty, &AstType::Bool)?;
                self.unify(true, &ty, &block.expression.ty)?;
                self.unify(true, &block.expression.ty, &else_block.expression.ty)?;
            }
            AstExpressionData::Match {
                expression,
                branches,
            } => {
                let match_expr_ty = &expression.ty;

                for AstMatchBranch {
                    pattern,
                    expression,
                } in branches
                {
                    self.full_unify_pattern(pattern, match_expr_ty)?;
                    self.unify(true, &expression.ty, &ty)?;
                }
            }
            AstExpressionData::Literal(lit) => match lit {
                AstLiteral::True | AstLiteral::False => {
                    self.unify(true, &ty, &AstType::Bool)?;
                }
                AstLiteral::String { .. } => self.unify(true, &ty, &AstType::String)?,
                AstLiteral::Int(..) => {
                    self.unify(true, &ty, &AstType::Int)?;
                }
                AstLiteral::Char(..) => {
                    self.unify(true, &ty, &AstType::Char)?;
                }
            },
            AstExpressionData::Identifier { variable_id, .. } => {
                // TODO: I should really split this and the actual type checker...
                let variable_ty = self.variables[variable_id.as_ref().unwrap()].clone();
                self.unify(true, &ty, &variable_ty)?;
            }
            AstExpressionData::GlobalVariable { name } => {
                self.unify(
                    true,
                    &ty,
                    &self.analyzed_program.clone().analyzed_globals[name],
                )?;
            }
            AstExpressionData::Tuple { values } => {
                let tuple_tys = into_types(values);
                self.unify(true, &ty, &AstType::tuple(tuple_tys))?;
            }
            AstExpressionData::ArrayLiteral { elements } => {
                let _tuple_tys = into_types(elements);
                let elem_ty = AstType::infer();

                for elem in elements {
                    self.unify(true, &elem.ty, &elem_ty)?;
                }

                self.unify(true, &AstType::array(elem_ty), &ty)?;
            }

            // A regular function call
            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => {
                let (param_tys, return_ty, objectives) =
                    instantiate_fn_signature(&self.analyzed_program, fn_name, generics)?;
                let arg_tys = into_types(args);
                self.unify_all(true, &param_tys, &arg_tys)?;
                self.unify(true, &return_ty, &ty)?;
                self.satisfy_restrictions(&objectives)?; // Add fn restrictions
            }
            // Call an object's static function
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                impl_signature,
            } => {
                if associated_trait.is_none() {
                    if let Some((trait_candidate, impl_candidate)) = self.satisfy_method(
                        call_type,
                        fn_name,
                        fn_generics,
                        &into_types(&args),
                        &ty,
                    )? {
                        *associated_trait = Some(trait_candidate);
                        *impl_signature = Some(impl_candidate);
                    }
                }

                if let Some(associated_trait) = associated_trait {
                    let fn_data = &self.analyzed_program.analyzed_traits
                        [&associated_trait.trt.name]
                        .methods[fn_name];

                    let expected_args = fn_data.parameters.len();
                    if args.len() != expected_args {
                        return PResult::error(format!(
                            "Incorrect number of arguments for \
                method `<{} as {}>:{}(...)`. Expected {}, found {}.",
                            call_type,
                            associated_trait,
                            fn_name,
                            expected_args,
                            args.len()
                        ));
                    }

                    let expected_generics = fn_data.generics.len();
                    if fn_generics.len() == expected_generics {
                        /* Don't do anything. */
                    } else if fn_generics.len() == 0 {
                        *fn_generics = (0..expected_generics).map(|_| AstType::infer()).collect();
                    } else {
                        return PResult::error(format!(
                            "Incorrect number of generics for symbol `<{} as {}>:{}(...)`. \
                Expected {}, found {}.",
                            call_type,
                            associated_trait,
                            fn_name,
                            expected_generics,
                            fn_generics.len()
                        ));
                    };

                    let (param_tys, return_ty, objectives) = instantiate_trait_fn_signature(
                        &*self.analyzed_program,
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
                        let impl_candidate = self.satisfy_impl(&call_type, &associated_trait)?;
                        *impl_signature = impl_candidate;
                    }
                }

                if let Some(impl_signature) = impl_signature {
                    if !self.analyzed_program.analyzed_impls[&impl_signature.impl_id].is_dummy {
                        let (param_tys, return_ty, objectives) = instantiate_impl_fn_signature(
                            &*self.analyzed_program,
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
            }
            // A tuple access `a:1`
            AstExpressionData::TupleAccess { accessible, idx } => {
                let tuple_ty = &accessible.ty;

                match tuple_ty {
                    AstType::Tuple { types } => {
                        if types.len() <= *idx {
                            return PResult::error(format!(
                                "Cannot access tuple `{}` at index {}",
                                tuple_ty, idx,
                            ));
                        }

                        self.unify(true, &ty, &types[*idx])?;
                    }
                    AstType::AssociatedType { .. } | AstType::Infer(_) => {
                        if top_epoch_mut!(self).ambiguous.is_none() {
                            top_epoch_mut!(self).ambiguous =
                                Some(format!("Ambiguous tuple access {}:{}", tuple_ty, idx));
                        }
                    }
                    t => {
                        return PResult::error(format!(
                            "Cannot perform tuple access on type `{}`",
                            t,
                        ));
                    }
                }
            }
            // Call an object's member
            AstExpressionData::ObjectAccess {
                object, mem_name, ..
            } => {
                let object_ty = &object.ty;

                match object_ty {
                    AstType::Object(name, generics) => {
                        if !self.analyzed_program.analyzed_objects[name]
                            .member_tys
                            .contains_key(mem_name)
                        {
                            return PResult::error(format!(
                                "Cannot access object `{}` at member `{}`",
                                object_ty, mem_name,
                            ));
                        }

                        let member_ty = instantiate_object_member(
                            &self.analyzed_program,
                            name,
                            generics,
                            mem_name,
                        )?;
                        self.unify(true, &ty, &member_ty)?;
                    }
                    AstType::AssociatedType { .. } | AstType::Infer(_) => {
                        if top_epoch_mut!(self).ambiguous.is_none() {
                            top_epoch_mut!(self).ambiguous = Some(format!(
                                "Ambiguous object access {}:{}",
                                object_ty, mem_name
                            ));
                        }
                    }
                    t => {
                        return PResult::error(format!(
                            "Cannot perform object access on type `{}`",
                            t,
                        ));
                    }
                }
            }

            AstExpressionData::AllocateObject {
                object,
                generics,
                children,
                ..
            } => {
                let expected_tys =
                    instantiate_object_members(&self.analyzed_program, object, &generics)?;

                for (child, expr) in children {
                    self.unify(true, &expected_tys[child], &expr.ty)?;
                }

                self.unify(
                    true,
                    &AstType::Object(object.clone(), generics.clone()),
                    &ty,
                )?;
            }

            AstExpressionData::Not(subexpression) => {
                let sub_ty = &subexpression.ty;
                self.unify(true, sub_ty, &AstType::Bool)?;
                self.unify(true, &ty, &AstType::Bool)?;
            }
            AstExpressionData::Negate(subexpression) => {
                let sub_ty = &subexpression.ty;
                self.unify(true, sub_ty, &AstType::Int)?;
                self.unify(true, &ty, &AstType::Int)?;
            }

            AstExpressionData::Assign { lhs, rhs } => {
                let lhs_ty = &lhs.ty;
                let rhs_ty = &rhs.ty;
                self.unify(true, lhs_ty, rhs_ty)?;
                self.unify(true, lhs_ty, &ty)?;
            }

            AstExpressionData::GlobalFn { name } => {
                let fn_data = &self.analyzed_program.analyzed_functions[name];
                let fn_ptr_ty =
                    AstType::fn_ptr_type(fn_data.parameters.clone(), fn_data.return_type.clone());
                self.unify(true, &ty, &fn_ptr_ty)?;
            }

            AstExpressionData::Closure {
                params,
                expr,
                variables,
                ..
            } => {
                self.variables.extend(
                    variables
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|(&k, v)| (k, v.ty.clone())),
                );

                let ret_ty = expr.ty.clone();
                let param_tys = params.iter().map(|p| p.ty.clone()).collect();
                self.unify(true, &ty, &AstType::closure_type(param_tys, ret_ty))?;
            }

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

                let expected_tys = instantiate_enum_pattern(
                    &self.analyzed_program,
                    enumerable,
                    &generics,
                    &variant,
                )?;

                for (child, ty) in
                    ZipExact::zip_exact(children, expected_tys, "positional elements")?
                {
                    self.unify(true, &child.ty, &ty)?;
                }
            }

            AstExpressionData::SelfRef
            | AstExpressionData::AllocateArray { .. }
            | AstExpressionData::ExprCall { .. }
            | AstExpressionData::ObjectCall { .. }
            | AstExpressionData::ArrayAccess { .. }
            | AstExpressionData::NamedEnum { .. }
            | AstExpressionData::PlainEnum { .. }
            | AstExpressionData::BinOp { .. } => unreachable!(),
        }

        Ok(AstExpression { data, ty, span })
    }

    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        match self.normalize_ty(t)? {
            AstType::Object(name, generics) => {
                self.satisfy_well_formed_object(&name, &generics)?;
                Ok(AstType::Object(name, generics))
            }
            AstType::Enum(name, generics) => {
                self.satisfy_well_formed_enum(&name, &generics)?;
                Ok(AstType::Enum(name, generics))
            }
            t => Ok(t),
        }
    }

    fn exit_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        self.full_unify_pattern(&p, &AstType::infer())?;

        Ok(p)
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.return_type = None;
        self.variables.clear();

        Ok(f)
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.return_type = None;
        self.variables.clear();

        Ok(o)
    }
}

impl TyckAdapter for TyckSolver {
    fn exit_tyck_object_fn(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        let TyckInstantiatedObjectFunction {
            fun,
            impl_ty,
            trait_ty,
            fn_generics,
        } = &i;

        let (expected_params, expected_ret_ty, expected_constraints) =
            instantiate_trait_fn_signature(
                &self.analyzed_program,
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
                        "The impl method has an additional constraint \
                    not specified in the trait prototype: \n{:?}\n{:?}",
                        expected_constraints, given_constraints
                    ));
                    break;
                }
            }

            /* NOTE: I don't particularly care if the constraints are subset here... I THINK.

            for c in &expected_constraints {
                if !given_constraints.contains(c) {
                    return PResult::error(format!(
                        "The impl method has an additional \
                    constraint not specified in the trait prototype"
                    ));
                }
            } */
        }

        Ok(i)
    }
}

impl AnAdapter for TyckSolver {
    fn enter_analyzed_impl(&mut self, i: AnImplData) -> PResult<AnImplData> {
        assert!(i.is_dummy);

        self.satisfy_restrictions(&instantiate_trait_restrictions(
            &self.analyzed_program,
            &i.trait_ty,
            &i.impl_ty,
        )?)?;

        for (name, ty) in &i.associated_tys {
            let restrictions: Vec<_> = instantiate_associated_ty_restrictions(
                &self.analyzed_program,
                &i.trait_ty,
                &name,
                &ty,
            )?
            .into_iter()
            .map(|trt| AstTypeRestriction::new(ty.clone(), trt))
            .collect();

            self.satisfy_restrictions(&restrictions)?;
        }

        Ok(i)
    }
}

fn into_types(values: &[AstExpression]) -> Vec<AstType> {
    values.iter().map(|e| e.ty.clone()).collect()
}

struct NormalizationAdapter<'a>(&'a mut TyckSolver);

impl<'a> AstAdapter for NormalizationAdapter<'a> {
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Infer(id) => {
                if let Some(t) = top_epoch!(self.0).inferences.get(&id) {
                    t.clone().visit(self)
                } else {
                    Ok(AstType::Infer(id))
                }
            }
            AstType::AssociatedType {
                obj_ty,
                trait_ty: None,
                name,
            } => {
                if let Some(imp) = self.0.satisfy_associated_type(&obj_ty, &name)? {
                    instantiate_associated_ty(
                        &self.0.analyzed_program,
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
                }
            }
            AstType::AssociatedType {
                obj_ty,
                trait_ty: Some(trait_ty),
                name,
            } => {
                if let Some(imp) = self.0.satisfy_impl(&obj_ty, &trait_ty)? {
                    instantiate_associated_ty(
                        &self.0.analyzed_program,
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
                }
            }
            t => Ok(t),
        }
    }
}

pub struct TypeAmbiguityAdapter<'a>(&'a mut TyckSolver);

impl<'a> AstAdapter for TypeAmbiguityAdapter<'a> {
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
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
