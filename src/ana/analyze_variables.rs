use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{Expect, FileId, IntoError, PResult, StackMap, Visit};
use std::collections::{HashMap, HashSet};

pub struct AnalyzeVariables {
    analyzed_program: AnalyzedProgram,
    all_variables: StackMap<VariableId, AstNamedVariable>,
    scope: StackMap<String, VariableId>,

    global_variables: HashMap<String, FileId>,
}

impl PureAnalysisPass for AnalyzeVariables {
    fn new(analyzed_program: AnalyzedProgram) -> PResult<AnalyzeVariables> {
        Ok(AnalyzeVariables {
            analyzed_program,
            all_variables: StackMap::new(),
            scope: StackMap::new(),
            global_variables: HashMap::new(),
        })
    }

    fn drop(self) -> AnalyzedProgram {
        self.analyzed_program
    }
}

impl AnalyzeVariables {
    /**
     * Assigns an index to the AstNamedVariable, then inserts it into the scope
     * and into the `variables` map.
     */
    fn assign_index(&mut self, a: &AstNamedVariable) -> PResult<()> {
        let AstNamedVariable { span, name, id, .. } = a;

        self.scope
            .get_top(name)
            .is_not_expected(*span, "variable", &name)?;

        self.all_variables.add(*id, a.clone());
        self.scope.add(name.clone(), *id);

        Ok(())
    }
}

impl<'a> AstAdapter for AnalyzeVariables {
    fn enter_module(&mut self, m: AstModule) -> PResult<AstModule> {
        self.global_variables.clear();

        let mm = self.analyzed_program.analyzed_modules[&m.id].clone();
        let mm = (*mm).borrow();

        for (name, id) in mm.top_level_symbols() {
            self.global_variables.insert(name, id);
        }

        Ok(m)
    }

    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.push();
        self.all_variables.push();

        if !f.variables.is_empty() {
            return PResult::error_at(
                f.name_span,
                format!("Function `{}` has already been analyzed...", f.name),
            );
        }

        for x in &f.parameter_list {
            self.assign_index(x)?;
        }

        Ok(f)
    }

    fn enter_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.scope.push();
        Ok(b)
    }

    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match s {
            AstStatement::Let { pattern, value } => {
                let value = value.visit(self)?;

                Ok(AstStatement::Let { pattern, value })
            }
            s => Ok(s),
        }
    }

    fn enter_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        match &p.data {
            AstMatchPatternData::Identifier(name) => {
                self.assign_index(name)?;
            }
            _ => {}
        }

        Ok(p)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Identifier {
                name,
                variable_id: None,
            } => {
                if let Some(id) = self.scope.get(&name) {
                    AstExpressionData::Identifier {
                        name,
                        variable_id: Some(id),
                    }
                } else if let Some(id) = self.global_variables.get(&name) {
                    AstExpressionData::GlobalVariable {
                        name: ModuleRef::Normalized(*id, name),
                    }
                } else {
                    return PResult::error(format!("Cannot find variable by name: `{}`", name));
                }
            }
            AstExpressionData::Closure {
                params,
                expr,
                captured: None,
                variables: None,
            } => {
                let names = self.scope.keys();
                let mut i = CaptureIdentifier::new(names);

                for p in &params {
                    i.ignore(&p.name);
                }

                let expr = expr.visit(&mut i)?;
                let mut captured = Vec::new();

                self.scope.push();
                self.all_variables.push();

                for p in &params {
                    self.assign_index(&p)?;
                }

                for c in i.captured {
                    let old = self
                        .all_variables
                        .get(&self.scope.get(&c).unwrap())
                        .unwrap();
                    let new = AstNamedVariable::new(old.span, old.name.clone(), old.ty.clone());

                    self.assign_index(&new)?;
                    captured.push((old, new));
                }

                AstExpressionData::Closure {
                    params,
                    expr,
                    captured: Some(captured),
                    variables: None,
                }
            }
            AstExpressionData::SelfRef => {
                let variable_id = Some(
                    self.scope
                        .get("self" /* <- TODO: ew */)
                        .is_expected(e.span, "variable", "self")?,
                );
                AstExpressionData::Identifier {
                    name: "self".into(),
                    variable_id,
                }
            }
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn exit_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Closure {
                params,
                expr,
                captured,
                variables: None,
            } => {
                let variables = Some(self.all_variables.pop());
                self.scope.pop();

                AstExpressionData::Closure {
                    params,
                    expr,
                    captured,
                    variables,
                }
            }
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.push();
        self.all_variables.push();

        if !f.variables.is_empty() {
            return PResult::error_at(
                f.name_span,
                format!("Method `{}` has already been analyzed...", f.name),
            );
        }

        for x in &f.parameter_list {
            self.assign_index(x)?;
        }

        Ok(f)
    }

    fn exit_module(&mut self, a: AstModule) -> PResult<AstModule> {
        self.global_variables.clear();
        Ok(a)
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        let variables = self.all_variables.pop();
        self.analyzed_program.variable_ids.extend(variables.clone());
        self.scope.pop();

        Ok(AstFunction { variables, ..f })
    }

    fn exit_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.scope.pop();

        Ok(b)
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        let variables = self.all_variables.pop();
        self.analyzed_program.variable_ids.extend(variables.clone());

        self.scope.pop();
        Ok(AstObjectFunction { variables, ..o })
    }

    fn enter_match_branch(&mut self, b: AstMatchBranch) -> PResult<AstMatchBranch> {
        self.scope.push();
        Ok(b)
    }

    fn exit_match_branch(&mut self, b: AstMatchBranch) -> PResult<AstMatchBranch> {
        self.scope.pop();
        Ok(b)
    }
}

struct CaptureIdentifier {
    candidates: HashSet<String>,
    captured: HashSet<String>,
}

impl CaptureIdentifier {
    fn new(candidates: HashSet<String>) -> CaptureIdentifier {
        CaptureIdentifier {
            candidates,
            captured: HashSet::new(),
        }
    }

    fn ignore(&mut self, candidate: &str) {
        self.candidates.remove(candidate);
    }

    fn try_capture(&mut self, candidate: &str) {
        if self.candidates.contains(candidate) {
            self.captured.insert(candidate.into());
        }
    }
}

impl AstAdapter for CaptureIdentifier {
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::Identifier {
                name,
                variable_id: None,
            } => {
                self.try_capture(&name);
            }
            AstExpressionData::Closure { params, .. } => {
                for p in params {
                    self.ignore(&p.name);
                }
            }
            _ => {}
        }

        Ok(e)
    }

    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match s {
            AstStatement::Let { pattern, value } => {
                // Still need to detect if we capture in the value, e.g. `let x = x.`
                // This will ensure we do this before visiting the match pattern.
                let value = value.visit(self)?;

                Ok(AstStatement::Let { pattern, value })
            }
            s => Ok(s),
        }
    }

    fn enter_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        match &p.data {
            AstMatchPatternData::Identifier(name) => {
                self.ignore(&name.name);
            }
            _ => {}
        }

        Ok(p)
    }
}
