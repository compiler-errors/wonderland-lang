use crate::ana::represent::*;
use crate::parser::ast::AstProgram;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{PResult, Visit};

pub type AnalysisPassFn =
    Box<dyn FnOnce(AnalyzedProgram, AstProgram) -> PResult<(AnalyzedProgram, AstProgram)>>;

pub trait AstAnalysisPass: AstAdapter + Sized {
    fn new() -> Self;

    fn analyze(a: AnalyzedProgram, p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
        let p = p.visit(&mut Self::new())?;

        Ok((a, p))
    }
}

pub trait PureAnalysisPass: AstAdapter + Sized {
    fn new(a: AnalyzedProgram) -> PResult<Self>;
    fn drop(self) -> AnalyzedProgram;

    fn analyze(a: AnalyzedProgram, p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
        let mut pass = Self::new(a)?;
        let p = p.visit(&mut pass)?;
        let a = pass.drop();

        Ok((a, p))
    }
}

// An analysis pass which mutably affects the AnalyzedProgram.
pub trait DirtyAnalysisPass: AnAdapter + Sized {
    fn new(a: &AnalyzedProgram) -> PResult<Self>;

    fn analyze(a: AnalyzedProgram, p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
        let a_copy = a.clone();
        let mut pass = Self::new(&a_copy)?;
        let a = a.visit(&mut pass)?;
        let p = p.visit(&mut pass)?;

        Ok((a, p))
    }
}

pub trait AnAdapter: AstAdapter {
    fn enter_analyzed_program(&mut self, p: AnalyzedProgram) -> PResult<AnalyzedProgram> {
        Ok(p)
    }

    fn enter_analyzed_function(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        Ok(f)
    }

    fn enter_analyzed_trait(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        Ok(t)
    }

    fn enter_analyzed_object(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        Ok(o)
    }

    fn enter_analyzed_impl(&mut self, i: AnImplData) -> PResult<AnImplData> {
        Ok(i)
    }

    fn exit_analyzed_program(&mut self, p: AnalyzedProgram) -> PResult<AnalyzedProgram> {
        Ok(p)
    }

    fn exit_analyzed_function(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        Ok(f)
    }

    fn exit_analyzed_trait(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        Ok(t)
    }

    fn exit_analyzed_object(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        Ok(o)
    }

    fn exit_analyzed_impl(&mut self, i: AnImplData) -> PResult<AnImplData> {
        Ok(i)
    }
}

impl<T: AnAdapter> Visit<T> for AnalyzedProgram {
    fn visit(self, adapter: &mut T) -> PResult<AnalyzedProgram> {
        let AnalyzedProgram {
            variable_ids,
            analyzed_functions,
            analyzed_traits,
            analyzed_objects,
            analyzed_impls,
        } = adapter.enter_analyzed_program(self)?;

        let i = AnalyzedProgram {
            variable_ids: variable_ids.visit(adapter)?,
            analyzed_functions: analyzed_functions.visit(adapter)?,
            analyzed_traits: analyzed_traits.visit(adapter)?,
            analyzed_objects: analyzed_objects.visit(adapter)?,
            analyzed_impls: analyzed_impls.visit(adapter)?,
        };

        adapter.exit_analyzed_program(i)
    }
}

impl<T: AnAdapter> Visit<T> for AnFunctionData {
    fn visit(self, adapter: &mut T) -> PResult<AnFunctionData> {
        let AnFunctionData {
            name,
            generics,
            parameters,
            return_type,
            restrictions,
        } = adapter.enter_analyzed_function(self)?;

        let i = AnFunctionData {
            name: name.visit(adapter)?,
            generics,
            parameters: parameters.visit(adapter)?,
            return_type: return_type.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_analyzed_function(i)
    }
}

impl<T: AnAdapter> Visit<T> for AnObjectData {
    fn visit(self, adapter: &mut T) -> PResult<AnObjectData> {
        let AnObjectData {
            name,
            self_type,
            generics,
            member_tys,
            member_indices,
            restrictions,
        } = adapter.enter_analyzed_object(self)?;

        let i = AnObjectData {
            name: name.visit(adapter)?,
            self_type: self_type.visit(adapter)?,
            generics,
            member_tys: member_tys.visit(adapter)?,
            member_indices,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_analyzed_object(i)
    }
}

impl<T: AnAdapter> Visit<T> for AnTraitData {
    fn visit(self, adapter: &mut T) -> PResult<AnTraitData> {
        let AnTraitData {
            name,
            generics,
            methods,
            associated_tys,
            restrictions,
            impls,
        } = adapter.enter_analyzed_trait(self)?;

        let i = AnTraitData {
            name: name.visit(adapter)?,
            generics,
            methods: methods.visit(adapter)?,
            associated_tys: associated_tys.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
            impls,
        };

        adapter.exit_analyzed_trait(i)
    }
}

impl<T: AnAdapter> Visit<T> for AnImplData {
    fn visit(self, adapter: &mut T) -> PResult<AnImplData> {
        let AnImplData {
            impl_id,
            generics,
            methods,
            trait_ty,
            impl_ty,
            restrictions,
            associated_tys,
            is_dummy,
        } = adapter.enter_analyzed_impl(self)?;

        let i = AnImplData {
            impl_id,
            generics,
            methods: methods.visit(adapter)?,
            trait_ty: trait_ty.visit(adapter)?,
            impl_ty: impl_ty.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
            associated_tys: associated_tys.visit(adapter)?,
            is_dummy,
        };

        adapter.exit_analyzed_impl(i)
    }
}
