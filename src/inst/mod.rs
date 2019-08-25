use crate::parser::ast::*;
use crate::analyze::represent::AnalyzedFile;
use std::rc::Rc;
use std::collections::{HashSet, HashMap};
use crate::util::result::{PResult, Expect};
use crate::parser::ast_visitor::{Adapter, Visit};
use crate::inst::represent::*;
use crate::tyck::*;
use crate::inst::post_solve::PostSolveAdapter;

mod represent;
mod post_solve;

struct InstantiationAdapter {
    analyzed_file: Rc<AnalyzedFile>,
    base_solver: TyckSolver,

    fns: HashMap<String, AstFunction>,
    objects: HashMap<String, AstObject>,
    impls: HashMap<ImplId, AstImpl>,
    obj_fns: HashMap<(ImplId, String), AstObjectFunction>,

    instantiated_fns: HashMap<InstFunctionSignature, Option<AstFunction>>,
    instantiated_object_fns: HashMap<InstObjectFunctionSignature, Option<AstObjectFunction>>,
    instantiated_impls: HashMap<InstImplSignature, Option<AstImpl>>,
    instantiated_objects: HashMap<InstObjectSignature, Option<AstObject>>,

    solved_impls: HashMap<TyckObjective, InstImplSignature>,
}

pub struct InstantiatedFile {
    instantiated_fns: HashMap<InstFunctionSignature, Option<AstFunction>>,
    instantiated_object_fns: HashMap<InstObjectFunctionSignature, Option<AstObjectFunction>>,
    instantiated_impls: HashMap<InstImplSignature, Option<AstImpl>>,
    instantiated_objects: HashMap<InstObjectSignature, Option<AstObject>>,
}

pub fn instantiate(parsed_file: ParsedFile, analyzed_file: AnalyzedFile) -> PResult<InstantiatedFile> {
    let fns: HashMap<_, _> = parsed_file.functions.into_iter().map(|f| (f.name.clone(), f)).collect();
    let objects: HashMap<_, _> = parsed_file.objects.into_iter().map(|o| (o.name.clone(), o)).collect();
    let impls: HashMap<_, _> = parsed_file.impls.into_iter().map(|i| (i.impl_id, i)).collect();

    let mut obj_fns = HashMap::new();
    for (&id, i) in &impls {
        for fun in &i.fns {
            obj_fns.insert((id, fun.name.clone()), fun.clone());
        }
    }

    let analyzed_file = Rc::new(analyzed_file);

    let mut i = InstantiationAdapter {
        base_solver: TyckSolver::new(analyzed_file.clone()),
        analyzed_file,

        fns,
        objects,
        impls,
        obj_fns,

        instantiated_fns: HashMap::new(),
        instantiated_object_fns: HashMap::new(),
        instantiated_impls: HashMap::new(),
        instantiated_objects: HashMap::new(),
        solved_impls: HashMap::new(),
    };

    i.instantiate_function(&"main".to_string(), &Vec::new())?;

    Ok(InstantiatedFile {
        instantiated_fns: i.instantiated_fns,
        instantiated_object_fns: i.instantiated_object_fns,
        instantiated_impls: i.instantiated_impls,
        instantiated_objects: i.instantiated_objects,
    })
}

impl InstantiationAdapter {
    fn instantiate_function(&mut self, name: &String, generics: &Vec<AstType>) -> PResult<()> {
        let sig = InstFunctionSignature(name.clone(), generics.clone());

        if self.instantiated_fns.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_fns.insert(sig.clone(), None);

        let ids = &self.analyzed_file.clone().analyzed_functions[name].generics;
        let f = self.process_simple(self.fns[name].clone(), ids, generics)?;

        self.instantiated_fns.insert(sig, Some(f));

        Ok(())
    }

    fn instantiate_object(&mut self, name: &String, generics: &Vec<AstType>) -> PResult<()> {
        let sig = InstObjectSignature(name.clone(), generics.clone());

        if self.instantiated_objects.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_objects.insert(sig.clone(), None);

        let ids = &self.analyzed_file.clone().analyzed_objects[name].generics;
        let o = self.process_simple(self.objects[name].clone(), ids, generics)?;

        self.instantiated_objects.insert(sig, Some(o));

        Ok(())
    }
    
    fn instantiate_impl(&mut self, sig: &TyckImplSignature, ty: &AstType, trt: &AstTraitType) -> PResult<()> {
        let id = sig.impl_id;
        let generics = &sig.generics;
        let sig = InstImplSignature(sig.impl_id, sig.generics.clone());
        
        if self.instantiated_impls.contains_key(&sig) {
            return Ok(());
        }
        
        self.instantiated_impls.insert(sig.clone(), None);
        let objective = TyckObjective { obj_ty: ty.clone(), trait_ty: trt.clone() };

        // Ensure that this is the only impl for this specific `<ty as trt>::...`
        self.solved_impls.insert(objective, sig.clone()).not_expected(self.impls[&id].name_span, "impl", "<conflicting types>")?;
        
        let ids = &self.analyzed_file.clone().analyzed_impls[&id].generics;
        let mut instantiate = GenericsInstantiator::from_generics(ids, generics)?;
        
        let mut imp = self.impls[&id].clone();
        imp.impl_ty = imp.impl_ty.visit(&mut instantiate)?;
        imp.trait_ty = imp.trait_ty.visit(&mut instantiate)?;
        imp.restrictions = imp.restrictions.visit(&mut instantiate)?;
        imp.associated_types = imp.associated_types.visit(&mut instantiate)?;
        
        let (mut imp, solution) = typecheck_impl(self.analyzed_file.clone(), &self.base_solver, imp)?;
        let mut post_solve = PostSolveAdapter(solution);

        imp.impl_ty = imp.impl_ty.visit(&mut post_solve)?.visit(self)?;
        imp.trait_ty = imp.trait_ty.visit(&mut post_solve)?.visit(self)?;
        imp.restrictions = imp.restrictions.visit(&mut post_solve)?.visit(self)?;
        imp.associated_types = imp.associated_types.visit(&mut post_solve)?.visit(self)?;

        self.instantiated_impls.insert(sig, Some(imp));
        
        Ok(())
    }

    fn instantiate_object_function(&mut self, call_type: &AstType, trt: &AstTraitType, impl_sig: &TyckImplSignature, fn_name: &String, fn_generics: &Vec<AstType>) -> PResult<()> {
        self.instantiate_impl(impl_sig, call_type, trt)?;

        let id = impl_sig.impl_id;
        let sig = InstObjectFunctionSignature(call_type.clone(), trt.clone(), fn_name.clone(), fn_generics.clone());

        if self.instantiated_object_fns.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_object_fns.insert(sig.clone(), None);

        let impl_data = &self.analyzed_file.clone().analyzed_impls[&id];
        let fn_data = &impl_data.methods[fn_name];

        let obj_ids = &impl_data.generics;
        let obj_generics = &impl_sig.generics;
        let fn_ids = &fn_data.generics;

        // Let's first instantiate it.
        let mut obj_instantiate = GenericsInstantiator::from_generics(obj_ids, obj_generics)?;
        let mut fn_instantiate = GenericsInstantiator::from_generics(fn_ids, fn_generics)?;
        let f = self.obj_fns[&(id, fn_name.clone())].clone().visit(&mut obj_instantiate)?.visit(&mut fn_instantiate)?;

        let (f, solution) = typecheck_impl_fn(self.analyzed_file.clone(), &self.base_solver, f, call_type, trt, fn_generics)?;
        let f = f.visit(&mut PostSolveAdapter(solution))?.visit(self)?;

        self.instantiated_object_fns.insert(sig, Some(f));

        Ok(())
    }

    fn process_simple<T>(&mut self, t: T, ids: &Vec<GenericId>, tys: &Vec<AstType>) -> PResult<T> where T: Visit<GenericsInstantiator> + Visit<TyckObjectiveAdapter> + Visit<PostSolveAdapter> + Visit<InstantiationAdapter> {
        // Let's first instantiate it.
        let mut instantiate = GenericsInstantiator::from_generics(ids, tys)?;
        let t = t.visit(&mut instantiate)?;

        let (t, solution) = typecheck_simple(self.analyzed_file.clone(), &self.base_solver, t)?;
        let t = t.visit(&mut PostSolveAdapter(solution))?.visit(self)?;

        Ok(t)
    }
}

impl Adapter for InstantiationAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::Object(name, generics) => {
                self.instantiate_object(name, generics)?;
            }
            _ => { /* Do nothing. */ }
        }

        Ok(t)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::Call { name, generics, .. } => {
                self.instantiate_function(name, generics)?;
            },
            AstExpressionData::StaticCall { call_type, associated_trait, impl_signature, fn_name, fn_generics, .. } => {
                let trt = associated_trait.as_ref().unwrap();
                let impl_sig = impl_signature.as_ref().unwrap();

                self.instantiate_object_function(call_type, trt, impl_sig, fn_name, fn_generics)?;
            }
            _ => { /* Do nothing. */ }
        }

        Ok(e)
    }
}