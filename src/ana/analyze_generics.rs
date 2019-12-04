use crate::ana::represent::*;
use crate::ana::represent_visitor::{AnAdapter, DirtyAnalysisPass};
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult, StackMap};

pub struct AnalyzeGenerics {
    pub scope: StackMap<String, GenericId>,
}

impl DirtyAnalysisPass for AnalyzeGenerics {
    fn new(_: &AnalyzedProgram) -> PResult<AnalyzeGenerics> {
        Ok(AnalyzeGenerics {
            scope: StackMap::new(),
        })
    }
}

impl AnalyzeGenerics {
    fn add_generics(&mut self, generics: &[AstGeneric]) -> PResult<()> {
        for g in generics {
            if self.scope.get(&g.1).is_some() {
                return PResult::error(format!("Duplicate generic with name `_{}`", g.1));
            }

            self.scope.add(g.1.clone(), g.0);
        }

        Ok(())
    }
}

impl AnAdapter for AnalyzeGenerics {
    fn enter_analyzed_function(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        self.scope.push();
        self.add_generics(&f.generics)?;
        Ok(f)
    }

    fn enter_analyzed_trait(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        self.scope.push();
        self.add_generics(&t.generics)?;
        Ok(t)
    }

    fn enter_analyzed_object(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        self.scope.push();
        self.add_generics(&o.generics)?;
        Ok(o)
    }

    fn enter_analyzed_enum(&mut self, e: AnEnumData) -> PResult<AnEnumData> {
        self.scope.push();
        self.add_generics(&e.generics)?;
        Ok(e)
    }

    fn enter_analyzed_impl(&mut self, i: AnImplData) -> PResult<AnImplData> {
        self.scope.push();
        self.add_generics(&i.generics)?;
        Ok(i)
    }

    fn exit_analyzed_function(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        self.scope.pop();
        Ok(f)
    }

    fn exit_analyzed_trait(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        self.scope.pop();
        Ok(t)
    }

    fn exit_analyzed_object(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        self.scope.pop();
        Ok(o)
    }

    fn exit_analyzed_enum(&mut self, e: AnEnumData) -> PResult<AnEnumData> {
        self.scope.pop();
        Ok(e)
    }

    fn exit_analyzed_impl(&mut self, i: AnImplData) -> PResult<AnImplData> {
        self.scope.pop();
        Ok(i)
    }
}

impl AstAdapter for AnalyzeGenerics {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::Generic(name) = t {
            if let Some(id) = self.scope.get(&name) {
                Ok(AstType::GenericPlaceholder(id, name))
            } else {
                PResult::error(format!("Cannot find generic with name `_{}`", name))
            }
        } else {
            Ok(t)
        }
    }

    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.push();
        self.add_generics(&f.generics)?;
        Ok(f)
    }

    fn enter_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.push();
        self.add_generics(&f.generics)?;
        Ok(f)
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.scope.push();
        self.add_generics(&t.generics)?;
        Ok(t)
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.scope.push();
        self.add_generics(&o.generics)?;
        Ok(o)
    }

    fn enter_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        self.scope.push();
        self.add_generics(&e.generics)?;
        Ok(e)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.scope.push();
        self.add_generics(&i.generics)?;
        Ok(i)
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.pop();
        Ok(f)
    }

    fn exit_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.pop();
        Ok(f)
    }

    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.scope.pop();
        Ok(t)
    }

    fn exit_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.scope.pop();
        Ok(o)
    }

    fn exit_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        self.scope.pop();
        Ok(e)
    }

    fn exit_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.scope.pop();
        Ok(i)
    }
}
