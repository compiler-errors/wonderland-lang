use crate::parser::ast::*;
use crate::parser::ast_visitor::{Adapter, Visit};
use crate::util::result::*;
use crate::util::StackMap;
use std::collections::HashMap;

pub struct VariableAdapter<'a> {
    variables: &'a mut HashMap<VariableId, AstNamedVariable>,

    scope: StackMap<String, VariableId>,
}

impl<'a> VariableAdapter<'a> {
    pub fn new(variables: &'a mut HashMap<VariableId, AstNamedVariable>) -> VariableAdapter<'a> {
        VariableAdapter {
            variables,
            scope: StackMap::new(),
        }
    }

    /**
     * Assigns an index to the AstNamedVariable, then inserts it into the scope
     * and into the `variables` map.
     */
    fn assign_index(&mut self, a: &AstNamedVariable) -> PResult<()> {
        let AstNamedVariable { span, name, ty, id } = a;
        println!("; Adding variable {:?} to scope {:?}", a, self.scope);

        self.scope
            .get_top(&name)
            .not_expected(*span, "variable", &name)?;

        let name = name.clone();
        self.variables
            .insert(*id, a.clone())
            .not_expected(*span, "variable", &name)?;
        self.scope.add(name, *id);

        Ok(())
    }
}

impl<'a> Adapter for VariableAdapter<'a> {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope.reset();
        self.variables.clear();

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

        for x in &parameter_list {
            self.assign_index(x)?;
        }

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
                let var = AstNamedVariable::new(name_span, var_name, ty);
                let value = value.visit(self)?;
                self.assign_index(&var)?;
                let id = AstExpression::identifier(name_span, var.name).visit(self)?;

                Ok(AstStatement::expression_statement(AstExpression::binop(
                    name_span.unite(value.span), /* Sum of LHS and RHS spans... */
                    id,
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
                println!(
                    "; Looking for variable {:?} in scope {:?}",
                    name, self.scope
                );
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
        self.variables.clear();

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

        for x in &parameter_list {
            self.assign_index(x)?;
        }

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

    fn exit_block(&mut self, mut b: AstBlock) -> PResult<AstBlock> {
        let leaving = self.scope.pop();
        b.locals.extend(leaving.values());

        Ok(b)
    }

    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        Ok(AstObjectFunction {
            variables: self.variables.clone(),
            ..o
        })
    }
}
