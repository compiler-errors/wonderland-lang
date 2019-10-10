use self::analyze_info::AnalyzeInfo;
use self::analyze_modules::{AnalyzeModules, AnalyzeUses};
use self::analyze_modules::{AnalyzePubUses, ModuleMap};
use self::represent::*;
use self::represent_visitor::*;
use crate::ana::analyze_generics::AnalyzeGenerics;
use crate::parser::ast::AstProgram;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{PResult, Visit};
use std::rc::Rc;

mod analyze_generics;
mod analyze_info;
mod analyze_modules;
pub mod represent;
pub mod represent_visitor;

pub fn analyze(p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
    // Before we jump into real analysis, let's first resolve imports.
    let mut mm = ModuleMap::new();

    let mut analyze_modules = AnalyzeModules::new(&mut mm);
    let mut p = p.visit(&mut analyze_modules)?;

    println!("Before pub uses: {:#?}", mm);

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

    println!("After pub uses: {:#?}", mm);

    let mut analyze_uses = AnalyzeUses::new(&mm);
    let p = p.visit(&mut analyze_uses)?;

    let mut analyze_info = AnalyzeInfo::new();

    let mut p = p.visit(&mut analyze_info)?;
    let mut a = analyze_info.analyzed_program;

    let passes: Vec<(&str, AnalysisPassFn)> =
        vec![("analyze_generics", Box::new(AnalyzeGenerics::analyze))];

    for (name, pass) in passes {
        let (a_new, p_new) = pass(a, p)?;

        a = a_new;
        p = p_new;
    }

    Ok((a, p))
}
