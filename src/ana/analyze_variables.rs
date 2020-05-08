use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    ast::{ast_visitor::AstAdapter, *},
    util::{Expect, FileId, PResult, StackMap, Visit},
};
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
     ** Assigns an index to the AstNamedVariable, then inserts it into the
     ** scope and into the `variables` map.
     **/
    fn assign_index(&mut self, a: &AstNamedVariable) -> PResult<()> {
        let AstNamedVariable { name, id, .. } = a;

        self.all_variables.add(*id, a.clone());
        self.scope.add(name.clone(), *id);

        Ok(())
    }
}

impl<'a> AstAdapter for AnalyzeVariables {
    fn enter_ast_module(&mut self, m: AstModule) -> PResult<AstModule> {
        self.global_variables.clear();

        let mm = self.analyzed_program.analyzed_modules[&m.id].clone();

        for (name, id) in (*mm).borrow().top_level_symbols() {
            self.global_variables.insert(name, id);
        }

        debug!("In module {}, {:?}", m.name, self.global_variables);

        Ok(m)
    }

    fn enter_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.push();
        self.all_variables.push();

        if !f.variables.is_empty() {
            return perror_at!(
                f.name_span,
                "Function `{}` has already been analyzed...",
                f.name,
            );
        }

        for x in &f.parameter_list {
            self.assign_index(x)?;
        }

        Ok(f)
    }

    fn enter_ast_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.scope.push();
        Ok(b)
    }

    fn enter_ast_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match s {
            AstStatement::Let { pattern, value } => {
                let value = value.visit(self)?;

                Ok(AstStatement::Let { pattern, value })
            },
            s => Ok(s),
        }
    }

    fn enter_ast_match_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        match &p.data {
            AstMatchPatternData::Identifier(name) => {
                self.assign_index(name)?;
            },
            _ => {},
        }

        Ok(p)
    }

    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Identifier {
                name,
                variable_id: None,
            } =>
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
                    return perror_at!(e.span, "Cannot find variable by name: `{}`", name);
                },
            AstExpressionData::Closure {
                params,
                expr,
                return_ty,
                captured: None,
                ..
            } => {
                let names = self.scope.keys();
                debug!("");
                debug!("Candidates: {:?}", names);
                let mut i = CaptureIdentifier::new(names);

                // Visit the params with our CaptureIdentifier. Since these params are type
                // AstMatchPattern, this will implicitly ignore() any identifiers declared here.
                let params = params.visit(&mut i)?;

                let expr = expr.visit(&mut i)?;
                let mut captured = Vec::new();

                self.scope.push();
                self.all_variables.push();

                // Now visit with the Self (AnalyzeVariables). This will call assign_index on
                // all of these parameters.
                let params = params.visit(self)?;

                for c in i.captured {
                    let old = self
                        .all_variables
                        .get(&self.scope.get(&c).unwrap())
                        .unwrap();
                    let new = AstNamedVariable::new(old.span, old.name.clone(), old.ty.clone());

                    self.assign_index(&new)?;
                    captured.push((old, new));
                }

                debug!("Captured: {:?}", captured);

                AstExpressionData::Closure {
                    params,
                    expr,
                    return_ty,
                    captured: Some(captured),
                    variables: None,
                }
            },
            AstExpressionData::SelfRef => {
                let variable_id = Some(
                    self.scope
                        .get("self" /* <- TODO: ew */)
                        .as_expected(e.span, "variable", "self")?,
                );
                AstExpressionData::Identifier {
                    name: "self".into(),
                    variable_id,
                }
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn exit_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Closure {
                params,
                return_ty,
                expr,
                captured,
                variables: None,
            } => {
                let variables = Some(self.all_variables.pop());
                self.scope.pop();

                AstExpressionData::Closure {
                    params,
                    return_ty,
                    expr,
                    captured,
                    variables,
                }
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_ast_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.push();
        self.all_variables.push();

        if !f.variables.is_empty() {
            return perror_at!(
                f.name_span,
                "Method `{}` has already been analyzed...",
                f.name,
            );
        }

        for x in &f.parameter_list {
            self.assign_index(x)?;
        }

        Ok(f)
    }

    fn exit_ast_module(&mut self, a: AstModule) -> PResult<AstModule> {
        self.global_variables.clear();
        Ok(a)
    }

    fn exit_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        let variables = self.all_variables.pop();
        self.analyzed_program.variable_ids.extend(variables.clone());
        self.scope.pop();

        Ok(AstFunction { variables, ..f })
    }

    fn exit_ast_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.scope.pop();

        Ok(b)
    }

    fn exit_ast_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        let variables = self.all_variables.pop();
        self.analyzed_program.variable_ids.extend(variables.clone());

        self.scope.pop();
        Ok(AstObjectFunction { variables, ..o })
    }

    fn enter_ast_match_branch(&mut self, b: AstMatchBranch) -> PResult<AstMatchBranch> {
        self.scope.push();
        Ok(b)
    }

    fn exit_ast_match_branch(&mut self, b: AstMatchBranch) -> PResult<AstMatchBranch> {
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
            debug!("Captured {}: {:?}", candidate, self.candidates);
            self.captured.insert(candidate.into());
        } else {
            debug!("Capture of {} failed: {:?}", candidate, self.candidates);
        }
    }
}

impl AstAdapter for CaptureIdentifier {
    fn enter_ast_expression(&mut self, mut e: AstExpression) -> PResult<AstExpression> {
        match &mut e.data {
            AstExpressionData::Identifier {
                name,
                variable_id: None,
            } => {
                self.try_capture(&name);
            },
            AstExpressionData::Closure { params, .. } => {
                *params = std::mem::take(params).visit(self)?;
            },
            _ => {},
        }

        Ok(e)
    }

    fn enter_ast_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match s {
            AstStatement::Let { pattern, value } => {
                // Still need to detect if we capture in the value, e.g. `let x = x.`
                // This will ensure we do this before visiting the match pattern.
                let value = value.visit(self)?;

                Ok(AstStatement::Let { pattern, value })
            },
            s => Ok(s),
        }
    }

    fn enter_ast_match_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        match &p.data {
            AstMatchPatternData::Identifier(name) => {
                debug!("Ignoring variable {:?}", name);
                self.ignore(&name.name);
            },
            _ => {},
        }

        Ok(p)
    }
}
