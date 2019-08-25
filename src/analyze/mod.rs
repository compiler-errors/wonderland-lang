use self::analyze_generics::GenericsAdapter;
use self::analyze_self::SelfAdapter;
use self::analyze_variables::VariableAdapter;

use crate::analyze::analyze_traits::TraitsAdapter;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::util::result::PResult;

use self::represent::*;
use crate::analyze::analyze_info::InfoAdapter;
use std::collections::HashMap;

mod analyze_generics;
mod analyze_info;
mod analyze_self;
mod analyze_traits;
mod analyze_variables;
pub mod represent;

// TODO: I need to verify a dependency graph for these analysis steps. I don't want cycles.

pub fn analyze(mut parsed_file: ParsedFile) -> PResult<(ParsedFile, AnalyzedFile)> {
    let mut method_to_trait = HashMap::new();
    let mut type_to_trait = HashMap::new();
    let mut function_generic_count = HashMap::new();
    let mut trait_generic_count = HashMap::new();
    let mut trait_method_generic_count = HashMap::new();
    let mut object_generic_count = HashMap::new();
    let mut variable_ids = HashMap::new();

    let mut self_adapter = SelfAdapter::new();
    parsed_file = parsed_file.visit(&mut self_adapter)?;

    let mut generics_adapter = GenericsAdapter::new(
        &mut function_generic_count,
        &mut trait_generic_count,
        &mut trait_method_generic_count,
        &mut object_generic_count,
        &mut method_to_trait,
        &mut type_to_trait,
    );
    parsed_file = parsed_file.visit(&mut generics_adapter)?;

    let mut traits_adapter = TraitsAdapter::new(
        &function_generic_count,
        &trait_generic_count,
        &trait_method_generic_count,
        &object_generic_count,
        &method_to_trait,
        &type_to_trait,
    );
    parsed_file = parsed_file.visit(&mut traits_adapter)?;

    let mut variable_adapter = VariableAdapter::new(&mut variable_ids);
    parsed_file = parsed_file.visit(&mut variable_adapter)?;

    let mut info_adapter = InfoAdapter::new(variable_ids);
    parsed_file = parsed_file.visit(&mut info_adapter)?;

    Ok((parsed_file, info_adapter.into()))
}
