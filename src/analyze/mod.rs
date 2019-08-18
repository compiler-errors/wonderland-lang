use self::analyze_generics::GenericsAdapter;
use self::analyze_infer::InferAdapter;
use self::analyze_self::SelfAdapter;
use self::analyze_variables::VariableAdapter;

use crate::analyze::analyze_traits::TraitsAdapter;
use crate::analyze::represent::AnalyzedFile;
use crate::parser::{ParsedFile, Visit};
use crate::util::result::PResult;

mod analyze_generics;
mod analyze_infer;
mod analyze_self;
mod analyze_traits;
mod analyze_variables;
mod represent;

// TODO: Really, this doesn't need to be a struct...
pub struct Analyzer {
    parsed_file: ParsedFile,
}

impl Analyzer {
    pub fn new(parsed_file: ParsedFile) -> Analyzer {
        Analyzer { parsed_file }
    }

    pub fn analyze(self) -> PResult<AnalyzedFile> {
        let Analyzer { mut parsed_file } = self;

        let mut self_adapter = SelfAdapter::new();
        parsed_file = parsed_file.visit(&mut self_adapter)?;

        let mut generics_adapter = GenericsAdapter::new();
        parsed_file = parsed_file.visit(&mut generics_adapter)?;

        let mut traits_adapter = TraitsAdapter::new(
            &generics_adapter.functions,
            &generics_adapter.traits,
            &generics_adapter.objects,
            &generics_adapter.method_to_trait,
            &generics_adapter.type_to_trait,
        );
        parsed_file = parsed_file.visit(&mut traits_adapter)?;

        let mut infer_adapter = InferAdapter::new();
        parsed_file = parsed_file.visit(&mut infer_adapter)?;

        let mut variable_adapter = VariableAdapter::new();
        parsed_file = parsed_file.visit(&mut variable_adapter)?;

        let variable_ids = variable_adapter.variables;
        let analyzed_functions = generics_adapter.functions;
        let analyzed_traits = generics_adapter.traits;
        let analyzed_objects = generics_adapter.objects;
        let analyzed_impls = generics_adapter.impls;

        Ok(AnalyzedFile {
            parsed_file,
            variable_ids,
            analyzed_functions,
            analyzed_impls,
            analyzed_objects,
            analyzed_traits,
        })
    }
}
