use crate::parser::*;
use crate::util::result::{Expect, PError, PResult};
use crate::util::{Counter, StackMap};
use std::collections::HashMap;

struct VariableAdapter {
    scope: StackMap<String, usize>,
    variable_counter: Counter,
    variables: HashMap<usize, AstNamedVariable>,
}

impl VariableAdapter {
    /**
     * Assigns an index to the AstNamedVariable, then inserts it into the scope
     * and into the `variables` map.
     */
    fn assign_index(&mut self, a: AstNamedVariable) -> PResult<AstNamedVariable> {
        match a {
            AstNamedVariable { name, ty, id: None } => {
                self.scope.get_top(&name).not_expected("variable", &name)?;

                let id = self.variable_counter.next();
                let var = AstNamedVariable {
                    name,
                    ty,
                    id: Some(id),
                };

                let name = var.name.clone();
                self.variables
                    .insert(id, var.clone())
                    .not_expected("variable", &name)?;
                self.scope.add(name, id);

                Ok(var)
            }
            _ => PError::new(
                0,
                format!("Assigning a duplicate index to variable `{}`", &a.name),
            ),
        }
    }
}

impl Adapter for VariableAdapter {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.scope = StackMap::new();
        self.scope.push();

        let AstFunction {
            signature:
                AstFnSignature {
                    name,
                    generics,
                    parameter_list,
                    return_type,
                    restrictions,
                },
            definition,
        } = f;

        let parameter_list = parameter_list
            .into_iter()
            .map(|x| self.assign_index(x))
            .collect::<PResult<Vec<_>>>()?;

        Ok(AstFunction {
            signature: AstFnSignature {
                name,
                generics,
                parameter_list,
                return_type,
                restrictions,
            },
            definition,
        })
    }

    fn enter_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.scope.push();

        Ok(b)
    }

    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match s {
            AstStatement::Let {
                var_name,
                ty,
                value,
            } => {
                let var = self.assign_index(AstNamedVariable {
                    name: var_name,
                    ty,
                    id: None,
                })?;

                Ok(AstStatement::expression_statement(AstExpression::binop(
                    AstExpression::identifier(var.name),
                    value,
                    BinOpKind::Set,
                )))
            }
            s => Ok(s),
        }
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match e {
            AstExpression::Identifier { name } => {
                let idx = self.scope.get(&name).expected("variable", &name)?;
                Ok(AstExpression::VariableIdx(idx))
            }
            AstExpression::SelfRef => {
                let idx = self
                    .scope
                    .get(&"self".to_string() /* <- TODO: ew */)
                    .expected("variable", "self")?;
                Ok(AstExpression::VariableIdx(idx))
            }
            e => Ok(e),
        }
    }

    fn enter_object_function(&mut self, f: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.scope = StackMap::new();
        self.scope.push();

        let AstObjectFunction {
            signature:
                AstObjectFnSignature {
                    name,
                    generics,
                    has_self,
                    parameter_list,
                    return_type,
                    restrictions,
                },
            definition,
        } = f;

        if has_self {
            // TODO: Do I need to keep track of this???
            self.assign_index(AstNamedVariable {
                name: "self".to_string(),
                ty: AstType::SelfType,
                id: None, // for now..
            })?;
        }

        let parameter_list = parameter_list
            .into_iter()
            .map(|x| self.assign_index(x))
            .collect::<PResult<Vec<_>>>()?;

        Ok(AstObjectFunction {
            signature: AstObjectFnSignature {
                name,
                generics,
                has_self,
                parameter_list,
                return_type,
                restrictions,
            },
            definition,
        })
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        Ok(f)
    }

    fn exit_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.scope.pop();

        Ok(b)
    }
}
