use self::analyze_info::AnalyzeInfo;
use self::analyze_modules::{AnalyzeModules, AnalyzeUses};
use self::analyze_modules::{AnalyzePubUses, ModuleMap};
use self::represent::*;
use self::represent_visitor::*;
use crate::ana::analyze_argument_parity::AnalyzeArgumentParity;
use crate::ana::analyze_associated_types::AnalyzeAssociatedTypes;
use crate::ana::analyze_control_flow::AnalyzeControlFlow;
use crate::ana::analyze_elaborations::AnalyzeElaborations;
use crate::ana::analyze_enum_constructors::AnalyzeEnumConstructors;
use crate::ana::analyze_fn_calls::AnalyzeFnCalls;
use crate::ana::analyze_for_loops::AnalyzeForLoops;
use crate::ana::analyze_generics::AnalyzeGenerics;
use crate::ana::analyze_generics_parity::AnalyzeGenericsParity;
use crate::ana::analyze_global_names::AnalyzeGlobalNames;
use crate::ana::analyze_illegal_globals::AnalyzeIllegalGlobals;
use crate::ana::analyze_illegal_infers::AnalyzeIllegalInfers;
use crate::ana::analyze_impls::AnalyzeImpls;
use crate::ana::analyze_infallible_enums::AnalyzeInfallibleEnums;
use crate::ana::analyze_names::AnalyzeNames;
use crate::ana::analyze_object_functions::AnalyzeObjectFunctions;
use crate::ana::analyze_operators::AnalyzeOperators;
use crate::ana::analyze_positional_enums::AnalyzePositionalEnums;
use crate::ana::analyze_returns::AnalyzeReturns;
use crate::ana::analyze_self::AnalyzeSelf;
use crate::ana::analyze_variables::AnalyzeVariables;
use crate::parser::ast::AstProgram;
use crate::util::{Comment, PResult, Visit};

mod analyze_argument_parity;
mod analyze_associated_types;
mod analyze_control_flow;
mod analyze_elaborations;
mod analyze_enum_constructors;
mod analyze_fn_calls;
mod analyze_for_loops;
mod analyze_generics;
mod analyze_generics_parity;
mod analyze_global_names;
mod analyze_illegal_globals;
mod analyze_illegal_infers;
mod analyze_impls;
mod analyze_infallible_enums;
mod analyze_info;
mod analyze_modules;
mod analyze_names;
mod analyze_object_functions;
mod analyze_operators;
mod analyze_positional_enums;
mod analyze_returns;
mod analyze_self;
mod analyze_variables;
pub mod represent;
pub mod represent_visitor;

pub fn analyze(p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
    // Before we jump into real analysis, let's first resolve imports.
    let mut mm = ModuleMap::new();

    let mut analyze_modules = AnalyzeModules::new(&mut mm);
    let mut p = p.visit(&mut analyze_modules)?;

    loop {
        let mut analyze_pub_uses = AnalyzePubUses::new(&mut mm);
        p = p.visit(&mut analyze_pub_uses)?;

        if !analyze_pub_uses.modified {
            // We only really propagate errors up if they're not influenced by evolution of the imports.
            // This is so we can suppress temporary errors.
            if analyze_pub_uses.err.is_some() {
                return Err(analyze_pub_uses.err.unwrap());
            }

            break;
        }
    }

    let mut analyze_uses = AnalyzeUses::new(&mm);
    let p = p.visit(&mut analyze_uses)?;

    let mut analyze_info = AnalyzeInfo::new();

    let mut p = p.visit(&mut analyze_info)?;
    let mut a = analyze_info.analyzed_program;
    a.analyzed_modules = analyze_uses.modules;

    let passes: Vec<(&str, AnalysisPassFn)> = vec![
        ("analyze_names", Box::new(AnalyzeNames::analyze)),
        // All the syntactic sugar needs to go first.
        ("analyze_for_loops", Box::new(AnalyzeForLoops::analyze)),
        (
            "analyze_elaborations",
            Box::new(AnalyzeElaborations::analyze),
        ),
        // Then the rest.
        (
            "analyze_enum_constructors",
            Box::new(AnalyzeEnumConstructors::analyze),
        ),
        (
            "analyze_positional_enums",
            Box::new(AnalyzePositionalEnums::analyze),
        ),
        (
            "analyze_associated_types",
            Box::new(AnalyzeAssociatedTypes::analyze),
        ), // Before generics
        ("analyze_generics", Box::new(AnalyzeGenerics::analyze)),
        (
            "analyze_infallible_enums",
            Box::new(AnalyzeInfallibleEnums::analyze),
        ),
        ("analyze_variables", Box::new(AnalyzeVariables::analyze)),
        ("analyze_fn_calls", Box::new(AnalyzeFnCalls::analyze)),
        (
            "analyze_global_names",
            Box::new(AnalyzeGlobalNames::analyze),
        ),
        ("analyze_operators", Box::new(AnalyzeOperators::analyze)),
        (
            "analyze_control_flow",
            Box::new(AnalyzeControlFlow::analyze),
        ),
        ("analyze_impls", Box::new(AnalyzeImpls::analyze)), // Before object functions
        (
            "analyze_object_functions",
            Box::new(AnalyzeObjectFunctions::analyze),
        ),
        ("analyze_self", Box::new(AnalyzeSelf::analyze)),
        (
            "analyze_argument_parity",
            Box::new(AnalyzeArgumentParity::analyze),
        ),
        (
            "analyze_generics_parity",
            Box::new(AnalyzeGenericsParity::analyze),
        ),
        (
            "analyze_illegal_infers",
            Box::new(AnalyzeIllegalInfers::analyze),
        ),
        (
            "analyze_illegal_globals",
            Box::new(AnalyzeIllegalGlobals::analyze),
        ),
        ("analyze_returns", Box::new(AnalyzeReturns::analyze)),
    ];

    for (name, pass) in passes {
        let (a_new, p_new) = pass(a, p).with_comment(|| format!("In analysis pass: {}", name))?;

        a = a_new;
        p = p_new;
    }

    Ok((a, p))
}
