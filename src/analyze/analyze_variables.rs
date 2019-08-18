use crate::parser::*;
use crate::util::result::{Expect, PError, PResult};
use crate::util::{Counter, StackMap};
use std::collections::HashMap;

pub struct VariableAdapter {
    scope: StackMap<String, VariableId>,
    variable_counter: Counter,
    pub variables: HashMap<VariableId, AstNamedVariable>,
}

impl VariableAdapter {
    pub fn new() -> VariableAdapter {
        VariableAdapter {
            scope: StackMap::new(),
            variable_counter: Counter::new(0),
            variables: HashMap::new(),
        }
    }

    /**
     * Assigns an index to the AstNamedVariable, then inserts it into the scope
     * and into the `variables` map.
     */
    fn assign_index(&mut self, a: AstNamedVariable) -> PResult<AstNamedVariable> {
        match a {
            AstNamedVariable {
                span,
                name,
                ty,
                id: None,
            } => {
                self.scope
                    .get_top(&name)
                    .not_expected(span, "variable", &name)?;

                let id = VariableId(self.variable_counter.next());
                let var = AstNamedVariable {
                    span,
                    name,
                    ty,
                    id: Some(id),
                };

                let name = var.name.clone();
                self.variables
                    .insert(id, var.clone())
                    .not_expected(span, "variable", &name)?;
                self.scope.add(name, id);

                Ok(var)
            }
            AstNamedVariable { span, .. } => PError::new(
                span,
                format!("Assigning a duplicate index to variable `{}`", &a.name),
            ),
        }
    }
}

impl Adapter for VariableAdapter {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.reset();
        self.variables = HashMap::new();

        let AstFunction {
            name_span,
            name,
            generics,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables,
        } = f;

        if !variables.is_empty() {
            PError::new(
                name_span,
                format!("Function `{}` has already been analyzed...", name),
            )?;
        }

        let parameter_list = parameter_list
            .into_iter()
            .map(|x| self.assign_index(x))
            .collect::<PResult<Vec<_>>>()?;

        Ok(AstFunction {
            name_span,
            name,
            generics,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables,
        })
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
                let var = self.assign_index(AstNamedVariable {
                    span: name_span,
                    name: var_name,
                    ty,
                    id: None,
                })?;

                Ok(AstStatement::expression_statement(AstExpression::binop(
                    name_span.unite(value.span), /* Sum of LHS and RHS spans... */
                    AstExpression::identifier(name_span, var.name),
                    value,
                    BinOpKind::Set,
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
                let variable_id = Some(self.scope.get(&name).expected(e.span, "variable", &name)?);
                AstExpressionData::Identifier { name, variable_id }
            }
            AstExpressionData::SelfRef => {
                let variable_id = Some(
                    self.scope
                        .get(&"self".to_string() /* <- TODO: ew */)
                        .expected(e.span, "variable", "self")?,
                );
                AstExpressionData::Identifier {
                    name: "self".to_string(),
                    variable_id,
                }
            }
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope.reset();
        self.variables = HashMap::new();

        let AstObjectFunction {
            name_span,
            name,
            generics,
            has_self,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables,
        } = f;

        if !variables.is_empty() {
            PError::new(
                name_span,
                format!("Method `{}` has already been analyzed...", name),
            )?;
        }

        let parameter_list = parameter_list
            .into_iter()
            .map(|x| self.assign_index(x))
            .collect::<PResult<Vec<_>>>()?;

        Ok(AstObjectFunction {
            name_span,
            name,
            generics,
            has_self,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables,
        })
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        Ok(AstFunction {
            variables: self.variables.clone(),
            ..f
        })
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        Ok(AstObjectFunction {
            variables: self.variables.clone(),
            ..o
        })
    }

    fn exit_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.scope.pop();

        Ok(b)
    }
}
