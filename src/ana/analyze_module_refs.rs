use crate::{
    ana::{
        analyze_modules::{get_module, ModuleItem},
        represent::AnalyzedProgram,
        represent_visitor::PureAnalysisPass,
    },
    ast::{visitor::AstAdapter, ModuleRef},
    util::PResult,
};

pub struct AnalyzeModuleRefs {
    analyzed_program: AnalyzedProgram,
}

impl PureAnalysisPass for AnalyzeModuleRefs {
    fn new(analyzed_program: AnalyzedProgram) -> PResult<AnalyzeModuleRefs> {
        Ok(AnalyzeModuleRefs { analyzed_program })
    }

    fn drop(self) -> AnalyzedProgram {
        self.analyzed_program
    }
}

impl AstAdapter for AnalyzeModuleRefs {
    fn enter_module_ref(&mut self, m: ModuleRef) -> PResult<ModuleRef> {
        match m {
            ModuleRef::Denormalized(path) => {
                let (item, mod_path) = path.split_last().unwrap();
                let module_ref =
                    get_module(self.analyzed_program.top_module.as_ref().unwrap(), mod_path)?;
                let module = (*module_ref).borrow();

                if let ModuleItem::Symbol(file) = module.get_child(item, mod_path)? {
                    Ok(ModuleRef::Normalized(file, path.last().unwrap().clone()))
                } else {
                    perror!("Reference {} is not a symbol!", path.join("::"))
                }
            },
            n @ ModuleRef::Normalized(..) => Ok(n),
        }
    }
}
