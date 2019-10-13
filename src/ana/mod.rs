use self::analyze_info::AnalyzeInfo;
use self::analyze_modules::{AnalyzeModules, AnalyzeUses};
use self::analyze_modules::{AnalyzePubUses, ModuleMap};
use self::represent::*;
use self::represent_visitor::*;
use crate::ana::analyze_argument_parity::AnalyzeArgumentParity;
use crate::ana::analyze_associated_types::AnalyzeAssociatedTypes;
use crate::ana::analyze_binops::AnalyzeBinops;
use crate::ana::analyze_control_flow::AnalyzeControlFlow;
use crate::ana::analyze_generics::AnalyzeGenerics;
use crate::ana::analyze_generics_parity::AnalyzeGenericsParity;
use crate::ana::analyze_illegal_infers::AnalyzeIllegalInfers;
use crate::ana::analyze_impls::AnalyzeImpls;
use crate::ana::analyze_names::AnalyzeNames;
use crate::ana::analyze_object_functions::AnalyzeObjectFunctions;
use crate::ana::analyze_self::AnalyzeSelf;
use crate::ana::analyze_variables::AnalyzeVariables;
use crate::parser::ast::AstProgram;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{Comment, PResult, Visit};
use std::rc::Rc;

mod analyze_argument_parity;
mod analyze_associated_types;
mod analyze_binops;
mod analyze_control_flow;
mod analyze_generics;
mod analyze_generics_parity;
mod analyze_illegal_infers;
mod analyze_impls;
mod analyze_info;
mod analyze_modules;
mod analyze_names;
mod analyze_object_functions;
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

    println!("Program: {:#?}", a);

    let passes: Vec<(&str, AnalysisPassFn)> = vec![
        (
            "analyze_illegal_infers",
            Box::new(AnalyzeIllegalInfers::analyze),
        ),
        ("analyze_names", Box::new(AnalyzeNames::analyze)),
        (
            "analyze_associated_types",
            Box::new(AnalyzeAssociatedTypes::analyze),
        ), // Before generics
        (
            "analyze_generics_parity",
            Box::new(AnalyzeGenericsParity::analyze),
        ),
        ("analyze_generics", Box::new(AnalyzeGenerics::analyze)),
        (
            "analyze_control_flow",
            Box::new(AnalyzeControlFlow::analyze),
        ),
        ("analyze_impls", Box::new(AnalyzeImpls::analyze)), // Before analyze object functions
        (
            "analyze_object_functions",
            Box::new(AnalyzeObjectFunctions::analyze),
        ),
        ("analyze_variables", Box::new(AnalyzeVariables::analyze)),
        ("analyze_self", Box::new(AnalyzeSelf::analyze)),
        (
            "analyze_argument_parity",
            Box::new(AnalyzeArgumentParity::analyze),
        ),
        ("analyze_binops", Box::new(AnalyzeBinops::analyze)),
    ];

    for (name, pass) in passes {
        let (a_new, p_new) = pass(a, p).with_comment(format!("In analysis pass: {}", name))?;

        a = a_new;
        p = p_new;
    }

    Ok((a, p))
}
