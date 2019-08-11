use crate::parser::*;
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
    fn assign_index(&mut self, a: AstNamedVariable) -> AstNamedVariable {
        match a {
            AstNamedVariable { name, ty, id: None } => {
                if self.scope.get_top(&name).is_some() {
                    unimplemented!("this should be an error. duplicate")
                }

                let id = self.variable_counter.next();
                let var = AstNamedVariable {
                    name,
                    ty,
                    id: Some(id),
                };

                self.scope.add(var.name.clone(), id);
                self.variables.insert(id, var.clone());

                var
            }
            _ => panic!("TODO: This should be an error."),
        }
    }
}

impl Adapter for VariableAdapter {
    fn enter_function(&mut self, f: AstFunction) -> AstFunction {
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
            .collect();

        AstFunction {
            signature: AstFnSignature {
                name,
                generics,
                parameter_list,
                return_type,
                restrictions,
            },
            definition,
        }
    }

    fn enter_block(&mut self, b: AstBlock) -> AstBlock {
        self.scope.push();
        b
    }

    fn enter_statement(&mut self, s: AstStatement) -> AstStatement {
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
                });

                AstStatement::expression_statement(AstExpression::binop(
                    AstExpression::identifier(var.name),
                    value,
                    BinOpKind::Set,
                ))
            }
            s => s,
        }
    }

    fn enter_expression(&mut self, e: AstExpression) -> AstExpression {
        match e {
            AstExpression::Identifier { name } => {
                let idx = self.scope.get(&name).unwrap(); // TODO: Error...
                AstExpression::VariableIdx(idx)
            }
            AstExpression::SelfRef => {
                let idx = self.scope.get(&"self".to_string() /* <- ew */).unwrap();
                AstExpression::VariableIdx(idx)
            }
            e => e,
        }
    }

    fn enter_object_function(&mut self, f: AstObjectFunction) -> AstObjectFunction {
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
            self.assign_index(AstNamedVariable {
                name: "self".to_string(),
                ty: AstType::SelfType,
                id: None, // for now..
            });
        }

        let parameter_list = parameter_list
            .into_iter()
            .map(|x| self.assign_index(x))
            .collect();

        AstObjectFunction {
            signature: AstObjectFnSignature {
                name,
                generics,
                has_self,
                parameter_list,
                return_type,
                restrictions,
            },
            definition,
        }
    }

    fn exit_function(&mut self, f: AstFunction) -> AstFunction {
        f
    }

    fn exit_block(&mut self, b: AstBlock) -> AstBlock {
        self.scope.pop();
        b
    }
}
