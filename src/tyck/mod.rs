use crate::analyze::represent::*;
use crate::parser::ast::*;
use crate::tyck::tyck_solver::TyckSolver;
use crate::util::result::*;

mod tyck_fn_walker;
mod tyck_instantiate;
mod tyck_solver;

/** An objective. Essentially captures the question:
 * Does obj_ty implement trait_ty (with given generics). */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyckObjective {
    obj_ty: AstType,
    trait_ty: AstTraitType,
}

#[derive(Debug, Clone)]
pub struct TyckImplSignature {
    impl_id: ImplId,
    generics: Vec<AstType>,
}

fn typecheck(file: AnalyzedFile) -> PResult<()> {
    unimplemented!()
}
