use crate::{
    ana::{
        represent::*,
        represent_visitor::{AnAdapter, DirtyAnalysisPass},
    },
    parser::{ast::*, ast_visitor::AstAdapter},
    util::{PResult, StackMap},
};

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
                return perror!("Duplicate generic with name `_{}`", g.1);
            }

            self.scope.add(g.1.clone(), g.0);
        }

        Ok(())
    }
}

impl AnAdapter for AnalyzeGenerics {
    fn enter_an_function_data(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        self.scope.push();
        self.add_generics(&f.generics)?;
        Ok(f)
    }

    fn enter_an_trait_data(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        self.scope.push();
        self.add_generics(&t.generics)?;
        Ok(t)
    }

    fn enter_an_object_data(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        self.scope.push();
        self.add_generics(&o.generics)?;
        Ok(o)
    }

    fn enter_an_enum_data(&mut self, e: AnEnumData) -> PResult<AnEnumData> {
        self.scope.push();
        self.add_generics(&e.generics)?;
        Ok(e)
    }

    fn enter_an_impl_data(&mut self, i: AnImplData) -> PResult<AnImplData> {
        self.scope.push();
        self.add_generics(&i.generics)?;
        Ok(i)
    }

    fn exit_an_function_data(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        self.scope.pop();
        Ok(f)
    }

    fn exit_an_trait_data(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        self.scope.pop();
        Ok(t)
    }

    fn exit_an_object_data(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        self.scope.pop();
        Ok(o)
    }

    fn exit_an_enum_data(&mut self, e: AnEnumData) -> PResult<AnEnumData> {
        self.scope.pop();
        Ok(e)
    }

    fn exit_an_impl_data(&mut self, i: AnImplData) -> PResult<AnImplData> {
        self.scope.pop();
        Ok(i)
    }
}

impl AstAdapter for AnalyzeGenerics {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::Generic(name) = t {
            if let Some(id) = self.scope.get(&name) {
                Ok(AstType::GenericPlaceholder(id, name))
            } else {
                perror!("Cannot find generic with name `_{}`", name)
            }
        } else {
            Ok(t)
        }
    }

    fn enter_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.push();
        self.add_generics(&f.generics)?;
        Ok(f)
    }

    fn enter_ast_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.push();
        self.add_generics(&f.generics)?;
        Ok(f)
    }

    fn enter_ast_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.scope.push();
        self.add_generics(&t.generics)?;
        Ok(t)
    }

    fn enter_ast_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.scope.push();
        self.add_generics(&o.generics)?;
        Ok(o)
    }

    fn enter_ast_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        self.scope.push();
        self.add_generics(&e.generics)?;
        Ok(e)
    }

    fn enter_ast_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.scope.push();
        self.add_generics(&i.generics)?;
        Ok(i)
    }

    fn exit_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.pop();
        Ok(f)
    }

    fn exit_ast_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.pop();
        Ok(f)
    }

    fn exit_ast_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.scope.pop();
        Ok(t)
    }

    fn exit_ast_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.scope.pop();
        Ok(o)
    }

    fn exit_ast_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        self.scope.pop();
        Ok(e)
    }

    fn exit_ast_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.scope.pop();
        Ok(i)
    }
}
