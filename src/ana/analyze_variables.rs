use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{Expect, IntoError, PResult, StackMap, Visit};
use std::collections::HashMap;

pub struct AnalyzeVariables {
    analyzed_program: AnalyzedProgram,
    fn_variables: HashMap<VariableId, AstNamedVariable>,
    scope: StackMap<String, VariableId>,
}

impl PureAnalysisPass for AnalyzeVariables {
    fn new(analyzed_program: AnalyzedProgram) -> PResult<AnalyzeVariables> {
        Ok(AnalyzeVariables {
            analyzed_program,
            fn_variables: HashMap::new(),
            scope: StackMap::new(),
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
        let AstNamedVariable {
            span,
            name,
            ty: _,
            id,
        } = a;

        self.scope
            .get_top(name)
            .is_not_expected(*span, "variable", &name)?;

        self.fn_variables
            .insert(*id, a.clone())
            .is_not_expected(*span, "variable", &name)?;
        self.scope.add(name.clone(), *id);

        Ok(())
    }
}

impl<'a> AstAdapter for AnalyzeVariables {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.reset();
        self.fn_variables.clear();

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
            AstStatement::Let {
                name_span,
                var_name,
                ty,
                value,
            } => {
                let var = AstNamedVariable::new(name_span, var_name, ty);
                let value = value.visit(self)?;
                self.assign_index(&var)?;
                let id = AstExpression::identifier(name_span, var.name).visit(self)?;

                Ok(AstStatement::expression_statement(AstExpression::assign(
                    name_span.unite(value.span), /* Sum of LHS and RHS spans... */
                    id,
                    value,
                )))
            }
            s => Ok(s),
        }
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Identifier {
                name,
                variable_id: None,
            } => {
                println!(
                    "; Looking for variable {:?} in scope {:?}",
                    name, self.scope
                );
                let variable_id = Some(
                    self.scope
                        .get(&name)
                        .is_expected(e.span, "variable", &name)?,
                );
                AstExpressionData::Identifier { name, variable_id }
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

    fn enter_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.reset();
        self.fn_variables.clear();

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

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        let variables = self.fn_variables.clone();
        self.analyzed_program
            .variable_ids
            .extend(self.fn_variables.drain());

        Ok(AstFunction { variables, ..f })
    }

    fn exit_block(&mut self, mut b: AstBlock) -> PResult<AstBlock> {
        let leaving = self.scope.pop();
        b.locals.extend(leaving.values());

        Ok(b)
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        let variables = self.fn_variables.clone();
        self.analyzed_program
            .variable_ids
            .extend(self.fn_variables.drain());

        Ok(AstObjectFunction { variables, ..o })
    }
}
