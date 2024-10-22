use crate::{
    ana::{
        analyze_argument_parity::AnalyzeArgumentParity,
        analyze_associated_types_and_methods::AnalyzeAssociatedTypesAndMethods,
        analyze_constructor_fields::AnalyzeConstructorFields,
        analyze_control_flow::AnalyzeControlFlow,
        analyze_elaborations::AnalyzeElaborations,
        analyze_fn_calls::AnalyzeFnCalls,
        analyze_for_loops::AnalyzeForLoops,
        analyze_generics::AnalyzeGenerics,
        analyze_generics_parity::AnalyzeGenericsParity,
        analyze_global_names::AnalyzeGlobalNames,
        analyze_illegal_globals::AnalyzeIllegalGlobals,
        analyze_illegal_infers::AnalyzeIllegalInfers,
        analyze_impls::AnalyzeImpls,
        analyze_infallible_enums::AnalyzeInfallibleEnums,
        analyze_info::AnalyzeInfo,
        analyze_module_refs::AnalyzeModuleRefs,
        analyze_modules::{AnalyzeModules, AnalyzePubUses, AnalyzeUses, ModuleMap},
        analyze_names::AnalyzeNames,
        analyze_object_indices::AnalyzeObjectIndices,
        analyze_operators::AnalyzeOperators,
        analyze_positional_enums::AnalyzePositionalEnums,
        analyze_self::AnalyzeSelf,
        analyze_variables::AnalyzeVariables,
        represent::*,
        represent_visitor::*,
    },
    ast::AstProgram,
    util::{Context, PResult, Visit},
};

mod analyze_info;
mod analyze_module_refs;
mod analyze_modules;

mod analyze_argument_parity;
mod analyze_associated_types_and_methods;
mod analyze_constructor_fields;
mod analyze_control_flow;
mod analyze_elaborations;
mod analyze_fn_calls;
mod analyze_for_loops;
mod analyze_generics;
mod analyze_generics_parity;
mod analyze_global_names;
mod analyze_illegal_globals;
mod analyze_illegal_infers;
mod analyze_impls;
mod analyze_infallible_enums;
mod analyze_names;
mod analyze_object_indices;
mod analyze_operators;
mod analyze_positional_enums;
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
            p = p.visit(&mut analyze_pub_uses)?;

            if !analyze_pub_uses.modified {
                // We only really propagate errors up if they're not influenced by evolution of
                // the imports. This is so we can suppress temporary errors.
                if analyze_pub_uses.err.is_some() {
                    return Err(analyze_pub_uses.err.unwrap());
                }

                break;
            }
        }
    }

    let mut analyze_uses = AnalyzeUses::new(&mm);
    let p = p.visit(&mut analyze_uses)?;
    let analyzed_uses_modules = analyze_uses.modules;

    let mut analyze_info = AnalyzeInfo::new(mm, analyzed_uses_modules);
    let p = p.visit(&mut analyze_info)?;
    let a = analyze_info.analyzed_program;

    analyze_item(a, p)
}

pub fn analyze_item_inplace<T>(a: &mut AnalyzedProgram, t: T) -> PResult<T>
where
    T: 'static
        + Visit<AnalyzeModuleRefs>
        + Visit<AnalyzeNames>
        + Visit<AnalyzeForLoops>
        + Visit<AnalyzeElaborations>
        + Visit<AnalyzeConstructorFields>
        + Visit<AnalyzeObjectIndices>
        + Visit<AnalyzePositionalEnums>
        + Visit<AnalyzeAssociatedTypesAndMethods>
        + Visit<AnalyzeGenerics>
        + Visit<AnalyzeInfallibleEnums>
        + Visit<AnalyzeVariables>
        + Visit<AnalyzeFnCalls>
        + Visit<AnalyzeGlobalNames>
        + Visit<AnalyzeOperators>
        + Visit<AnalyzeControlFlow>
        + Visit<AnalyzeImpls>
        + Visit<AnalyzeSelf>
        + Visit<AnalyzeArgumentParity>
        + Visit<AnalyzeGenericsParity>
        + Visit<AnalyzeIllegalGlobals>
        + Visit<AnalyzeIllegalInfers>,
{
    let a_clone = std::mem::replace(a, AnalyzedProgram::empty());
    let (a_clone, t) = analyze_item(a_clone, t)?;
    let _ = std::mem::replace(a, a_clone);

    Ok(t)
}

pub fn analyze_item<T>(mut a: AnalyzedProgram, mut t: T) -> PResult<(AnalyzedProgram, T)>
where
    T: 'static
        + Visit<AnalyzeModuleRefs>
        + Visit<AnalyzeNames>
        + Visit<AnalyzeForLoops>
        + Visit<AnalyzeElaborations>
        + Visit<AnalyzeConstructorFields>
        + Visit<AnalyzeObjectIndices>
        + Visit<AnalyzePositionalEnums>
        + Visit<AnalyzeAssociatedTypesAndMethods>
        + Visit<AnalyzeGenerics>
        + Visit<AnalyzeInfallibleEnums>
        + Visit<AnalyzeVariables>
        + Visit<AnalyzeFnCalls>
        + Visit<AnalyzeGlobalNames>
        + Visit<AnalyzeOperators>
        + Visit<AnalyzeControlFlow>
        + Visit<AnalyzeImpls>
        + Visit<AnalyzeSelf>
        + Visit<AnalyzeArgumentParity>
        + Visit<AnalyzeGenericsParity>
        + Visit<AnalyzeIllegalGlobals>
        + Visit<AnalyzeIllegalInfers>,
{
    let passes: Vec<(&str, AnalysisPassFn<T>)> = vec![
        ("analyze_module_refs", Box::new(AnalyzeModuleRefs::analyze)),
        ("analyze_names", Box::new(AnalyzeNames::analyze)),
        (
            "analyze_elaborations",
            Box::new(AnalyzeElaborations::analyze),
        ),
        // Then the rest.
        (
            "analyze_constructor_fields",
            Box::new(AnalyzeConstructorFields::analyze),
        ),
        (
            "analyze_object_indices",
            Box::new(AnalyzeObjectIndices::analyze),
        ),
        (
            "analyze_positional_enums",
            Box::new(AnalyzePositionalEnums::analyze),
        ),
        (
            "analyze_associated_types_and_methods",
            Box::new(AnalyzeAssociatedTypesAndMethods::analyze),
        ), // Before generics
        ("analyze_generics", Box::new(AnalyzeGenerics::analyze)),
        (
            "analyze_infallible_enums",
            Box::new(AnalyzeInfallibleEnums::analyze),
        ),
        ("analyze_variables", Box::new(AnalyzeVariables::analyze)),
        (
            "analyze_global_names",
            Box::new(AnalyzeGlobalNames::analyze),
        ),
        ("analyze_fn_calls", Box::new(AnalyzeFnCalls::analyze)),
        ("analyze_operators", Box::new(AnalyzeOperators::analyze)),
        (
            "analyze_control_flow",
            Box::new(AnalyzeControlFlow::analyze),
        ),
        ("analyze_impls", Box::new(AnalyzeImpls::analyze)), // Before object functions
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
        // All the syntactic sugar needs to go first.
        ("analyze_for_loops", Box::new(AnalyzeForLoops::analyze)),
    ];

    for (name, pass) in passes {
        let (a_new, p_new) = pass(a, t).with_comment(|| format!("In analysis pass: {}", name))?;

        a = a_new;
        t = p_new;
    }

    Ok((a, t))
}
