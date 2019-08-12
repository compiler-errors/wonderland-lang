use crate::parser::*;
use crate::util::result::{PError, PResult};
use std::borrow::BorrowMut;
use std::ops::DerefMut;

pub trait Visit<A>: Sized {
    fn visit(self, adapter: &mut A) -> PResult<Self>;
}

pub trait Adapter {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        Ok(f)
    }
    fn enter_fn_signature(&mut self, f: AstFnSignature) -> PResult<AstFnSignature> {
        Ok(f)
    }
    fn enter_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        Ok(b)
    }
    fn enter_named_variable(&mut self, p: AstNamedVariable) -> PResult<AstNamedVariable> {
        Ok(p)
    }
    fn enter_type_restriction(&mut self, t: AstTypeRestriction) -> PResult<AstTypeRestriction> {
        Ok(t)
    }
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(t)
    }
    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        Ok(s)
    }
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        Ok(e)
    }
    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        Ok(o)
    }
    fn enter_object_member(&mut self, o: AstObjectMember) -> PResult<AstObjectMember> {
        Ok(o)
    }
    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        Ok(o)
    }
    fn enter_object_fn_signature(
        &mut self,
        o: AstObjectFnSignature,
    ) -> PResult<AstObjectFnSignature> {
        Ok(o)
    }
    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        Ok(t)
    }
    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        Ok(i)
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        Ok(f)
    }
    fn exit_fn_signature(&mut self, f: AstFnSignature) -> PResult<AstFnSignature> {
        Ok(f)
    }
    fn exit_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        Ok(b)
    }
    fn exit_named_variable(&mut self, p: AstNamedVariable) -> PResult<AstNamedVariable> {
        Ok(p)
    }
    fn exit_type_restriction(&mut self, t: AstTypeRestriction) -> PResult<AstTypeRestriction> {
        Ok(t)
    }
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(t)
    }
    fn exit_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        Ok(s)
    }
    fn exit_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        Ok(e)
    }
    fn exit_object(&mut self, o: AstObject) -> PResult<AstObject> {
        Ok(o)
    }
    fn exit_object_member(&mut self, o: AstObjectMember) -> PResult<AstObjectMember> {
        Ok(o)
    }
    fn exit_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        Ok(o)
    }
    fn exit_object_fn_signature(
        &mut self,
        o: AstObjectFnSignature,
    ) -> PResult<AstObjectFnSignature> {
        Ok(o)
    }
    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        Ok(t)
    }
    fn exit_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        Ok(i)
    }
}

impl<T: Adapter, S: Visit<T>> Visit<T> for Box<S> {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        (*self).visit(adapter).map(Box::new)
    }
}

impl<T: Adapter, S: Visit<T>> Visit<T> for Vec<S> {
    fn visit(mut self, adapter: &mut T) -> PResult<Self> {
        self.into_iter()
            .map(|i| i.visit(adapter))
            .collect::<PResult<Vec<_>>>()
    }
}

impl<T: Adapter> Visit<T> for AstFunction {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstFunction {
            signature,
            definition,
        } = adapter.enter_function(self)?;

        let i = AstFunction {
            signature: signature.visit(adapter)?,
            definition: definition.visit(adapter)?,
        };

        adapter.exit_function(i)
    }
}

impl<T: Adapter> Visit<T> for AstFnSignature {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstFnSignature {
            name,
            generics,
            parameter_list,
            return_type,
            restrictions,
        } = adapter.enter_fn_signature(self)?;

        let i = AstFnSignature {
            name,
            generics,
            parameter_list: parameter_list.visit(adapter)?,
            return_type: return_type.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_fn_signature(i)
    }
}

impl<T: Adapter> Visit<T> for AstNamedVariable {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstNamedVariable { name, ty, id } = adapter.enter_named_variable(self)?;

        let i = AstNamedVariable {
            name,
            ty: ty.visit(adapter)?,
            id,
        };

        adapter.exit_named_variable(i)
    }
}

impl<T: Adapter> Visit<T> for AstTypeRestriction {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstTypeRestriction { ty, trt } = adapter.enter_type_restriction(self)?;

        let i = AstTypeRestriction {
            ty: ty.visit(adapter)?,
            trt: trt.visit(adapter)?,
        };

        adapter.exit_type_restriction(i)
    }
}

impl<T: Adapter> Visit<T> for AstBlock {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstBlock { statements } = adapter.enter_block(self)?;

        let i = AstBlock {
            statements: statements.visit(adapter)?,
        };

        adapter.exit_block(i)
    }
}

impl<T: Adapter> Visit<T> for AstStatement {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let stmt = match adapter.enter_statement(self)? {
            i @ AstStatement::Break | i @ AstStatement::Continue | i @ AstStatement::NoOp => i,
            AstStatement::Block { block } => AstStatement::Block {
                block: block.visit(adapter)?,
            },
            AstStatement::Let {
                var_name,
                ty,
                value,
            } => AstStatement::Let {
                var_name,
                ty: ty.visit(adapter)?,
                value: value.visit(adapter)?,
            },
            AstStatement::If {
                condition,
                block,
                else_block,
            } => AstStatement::If {
                condition: condition.visit(adapter)?,
                block: block.visit(adapter)?,
                else_block: else_block.visit(adapter)?,
            },
            AstStatement::While { condition, block } => AstStatement::While {
                condition: condition.visit(adapter)?,
                block: block.visit(adapter)?,
            },
            AstStatement::Return { value } => AstStatement::Return {
                value: value.visit(adapter)?,
            },
            AstStatement::Assert { condition } => AstStatement::Assert {
                condition: condition.visit(adapter)?,
            },
            AstStatement::Expression { expression } => AstStatement::Expression {
                expression: expression.visit(adapter)?,
            },
        };

        adapter.exit_statement(stmt)
    }
}

impl<T: Adapter> Visit<T> for AstExpression {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let expr = match adapter.enter_expression(self)? {
            i @ AstExpression::Nothing
            | i @ AstExpression::True
            | i @ AstExpression::False
            | i @ AstExpression::Null
            | i @ AstExpression::SelfRef
            | i @ AstExpression::String { .. }
            | i @ AstExpression::Int(..)
            | i @ AstExpression::Char(..)
            | i @ AstExpression::Identifier { .. }
            | i @ AstExpression::VariableIdx(..) => i,
            AstExpression::Tuple { values } => AstExpression::Tuple {
                values: values.visit(adapter)?,
            },
            AstExpression::Array { elements } => AstExpression::Array {
                elements: elements.visit(adapter)?,
            },
            AstExpression::Call {
                name,
                generics,
                args,
            } => AstExpression::Call {
                name,
                generics: generics.visit(adapter)?,
                args: args.visit(adapter)?,
            },
            AstExpression::ObjectCall {
                object,
                fn_name,
                generics,
                args,
            } => AstExpression::ObjectCall {
                object: object.visit(adapter)?,
                fn_name,
                generics: generics.visit(adapter)?,
                args: args.visit(adapter)?,
            },
            AstExpression::StaticCall {
                obj_name,
                obj_generics,
                fn_name,
                fn_generics,
                args,
            } => AstExpression::StaticCall {
                obj_name,
                obj_generics: obj_generics.visit(adapter)?,
                fn_name,
                fn_generics: fn_generics.visit(adapter)?,
                args: args.visit(adapter)?,
            },
            AstExpression::Access { accessible, idx } => AstExpression::Access {
                accessible: accessible.visit(adapter)?,
                idx: idx.visit(adapter)?,
            },
            AstExpression::TupleAccess { accessible, idx } => AstExpression::TupleAccess {
                accessible: accessible.visit(adapter)?,
                idx,
            },
            AstExpression::ObjectAccess { object, mem_name } => AstExpression::ObjectAccess {
                object: object.visit(adapter)?,
                mem_name,
            },
            AstExpression::Allocate { object } => AstExpression::Allocate {
                object: object.visit(adapter)?,
            },
            AstExpression::Not(expr) => AstExpression::Not(expr.visit(adapter)?),
            AstExpression::Negate(expr) => AstExpression::Negate(expr.visit(adapter)?),
            AstExpression::BinOp { kind, lhs, rhs } => AstExpression::BinOp {
                kind,
                lhs: lhs.visit(adapter)?,
                rhs: rhs.visit(adapter)?,
            },
        };

        adapter.exit_expression(expr)
    }
}

impl<T: Adapter> Visit<T> for AstType {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let ty = adapter.enter_type(self)?;

        let ty = match ty {
            i @ AstType::Infer
            | i @ AstType::Int
            | i @ AstType::Char
            | i @ AstType::Bool
            | i @ AstType::String
            | i @ AstType::Generic(..)
            | i @ AstType::InferPlaceholder(..)
            | i @ AstType::GenericPlaceholder(..) => i,
            AstType::Array { ty } => AstType::Array {
                ty: ty.visit(adapter)?,
            },
            AstType::Tuple { types } => AstType::Tuple {
                types: types.visit(adapter)?,
            },
            AstType::Object(name, types) => AstType::Object(name, types.visit(adapter)?),
            _ => unimplemented!(),
        };

        adapter.exit_type(ty)
    }
}

impl<T: Adapter> Visit<T> for AstObject {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstObject {
            generics,
            name,
            members,
            restrictions,
        } = adapter.enter_object(self)?;

        let i = AstObject {
            generics,
            name,
            members: members.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_object(i)
    }
}

impl<T: Adapter> Visit<T> for AstObjectFunction {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstObjectFunction {
            signature,
            definition,
        } = adapter.enter_object_function(self)?;

        let i = AstObjectFunction {
            signature: signature.visit(adapter)?,
            definition: definition.visit(adapter)?,
        };

        adapter.exit_object_function(i)
    }
}

impl<T: Adapter> Visit<T> for AstObjectFnSignature {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstObjectFnSignature {
            name,
            generics,
            has_self,
            parameter_list,
            return_type,
            restrictions,
        } = adapter.enter_object_fn_signature(self)?;

        let i = AstObjectFnSignature {
            name,
            generics,
            has_self,
            parameter_list: parameter_list.visit(adapter)?,
            return_type: return_type.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_object_fn_signature(i)
    }
}

impl<T: Adapter> Visit<T> for AstObjectMember {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstObjectMember { name, member_type } = adapter.enter_object_member(self)?;

        let i = AstObjectMember {
            name,
            member_type: member_type.visit(adapter)?,
        };

        adapter.exit_object_member(i)
    }
}

impl<T: Adapter> Visit<T> for AstTrait {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstTrait {
            name,
            generics,
            functions,
            restrictions,
        } = adapter.enter_trait(self)?;

        let i = AstTrait {
            name,
            generics,
            functions: functions.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_trait(i)
    }
}

impl<T: Adapter> Visit<T> for AstImpl {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstImpl {
            generics,
            trait_ty,
            impl_ty,
            fns,
            restrictions,
        } = adapter.enter_impl(self)?;

        let i = AstImpl {
            generics,
            trait_ty: trait_ty.visit(adapter)?,
            impl_ty: impl_ty.visit(adapter)?,
            fns: fns.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_impl(i)
    }
}
