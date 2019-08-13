use self::analyze_generics::GenericsAdapter;
use self::analyze_infer::InferAdapter;
use self::analyze_self::SelfAdapter;
use self::analyze_variables::VariableAdapter;
use crate::analyze::represent::AnalyzedFile;
use crate::parser::{Adapter, AstExpression, ParsedFile, Visit};
use crate::util::result::PResult;
use std::env::var;

mod analyze_generics;
mod analyze_infer;
mod analyze_self;
mod analyze_variables;
mod represent;

pub struct Analyzer {
    parsed_file: ParsedFile,

    infer_adapter: InferAdapter,
    generics_adapter: GenericsAdapter,
    self_adapter: SelfAdapter,
    variable_adapter: VariableAdapter,
}

impl Analyzer {
    pub fn new(parsed_file: ParsedFile) -> Analyzer {
        Analyzer {
            parsed_file,
            infer_adapter: InferAdapter::new(),
            generics_adapter: GenericsAdapter::new(),
            self_adapter: SelfAdapter::new(),
            variable_adapter: VariableAdapter::new(),
        }
    }

    pub fn analyze(self) -> PResult<()> {
        let Analyzer {
            parsed_file,
            mut variable_adapter,
            mut generics_adapter,
            mut infer_adapter,
            mut self_adapter,
        } = self;

        let parsed_file = parsed_file
            .visit(&mut generics_adapter)?
            .visit(&mut generics_adapter.second_pass())?
            .visit(&mut infer_adapter)?
            .visit(&mut self_adapter)?
            .visit(&mut variable_adapter)?;

        let variable_ids = variable_adapter.variables;
        let analyzed_functions = generics_adapter.functions;
        let analyzed_traits = generics_adapter.traits;
        let analyzed_objects = generics_adapter.objects;
        let analyzed_impls = generics_adapter.impls;

        Ok(())
    }
}
