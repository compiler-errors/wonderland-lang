use crate::parser::*;
use crate::util::result::PResult;
use crate::util::Span;
use std::collections::HashMap;
use std::env::var;
use std::hash::Hash;

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
    fn visit(self, adapter: &mut T) -> PResult<Vec<S>> {
        self.into_iter().map(|s| s.visit(adapter)).collect()
    }
}

impl<T: Adapter, K: Eq + Hash, V: Visit<T>> Visit<T> for HashMap<K, V> {
    fn visit(self, adapter: &mut T) -> PResult<HashMap<K, V>> {
        let mut out = HashMap::new();

        for (k, v) in self.into_iter() {
            out.insert(k, v.visit(adapter)?);
        }

        Ok(out)
    }
}

impl<T: Adapter> Visit<T> for ParsedFile {
    fn visit(self, adapter: &mut T) -> PResult<ParsedFile> {
        let ParsedFile {
            objects,
            traits,
            impls,
            export_fns,
            functions,
        } = self;

        let objects = objects.visit(adapter)?;
        let traits = traits.visit(adapter)?;
        let impls = impls.visit(adapter)?;
        let export_fns = export_fns.visit(adapter)?;
        let functions = functions.visit(adapter)?;

        Ok(ParsedFile {
            objects,
            traits,
            impls,
            export_fns,
            functions,
        })
    }
}

impl<T: Adapter> Visit<T> for AstFunction {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstFunction {
            signature,
            definition,
            variables,
        } = adapter.enter_function(self)?;

        let i = AstFunction {
            signature: signature.visit(adapter)?,
            definition: definition.visit(adapter)?,
            variables: variables.visit(adapter)?,
        };

        adapter.exit_function(i)
    }
}

impl<T: Adapter> Visit<T> for AstFnSignature {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstFnSignature {
            name_span,
            name,
            generics,
            parameter_list,
            return_type,
            restrictions,
        } = adapter.enter_fn_signature(self)?;

        let i = AstFnSignature {
            name_span,
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
        let AstNamedVariable { span, name, ty, id } = adapter.enter_named_variable(self)?;

        let i = AstNamedVariable {
            span,
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
                name_span,
                var_name,
                ty,
                value,
            } => AstStatement::Let {
                name_span,
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
        let AstExpression { data, ty, span } = adapter.enter_expression(self)?;

        let ty = if let Some(x) = ty {
            Some(x.visit(adapter)?)
        } else {
            None
        };

        let data = match data {
            i @ AstExpressionData::Nothing
            | i @ AstExpressionData::True
            | i @ AstExpressionData::False
            | i @ AstExpressionData::Null
            | i @ AstExpressionData::SelfRef
            | i @ AstExpressionData::String { .. }
            | i @ AstExpressionData::Int(..)
            | i @ AstExpressionData::Char(..)
            | i @ AstExpressionData::Identifier { .. }
            | i @ AstExpressionData::VariableIdx(..) => i,
            AstExpressionData::Tuple { values } => AstExpressionData::Tuple {
                values: values.visit(adapter)?,
            },
            AstExpressionData::Array { elements } => AstExpressionData::Array {
                elements: elements.visit(adapter)?,
            },
            AstExpressionData::Call {
                name,
                generics,
                args,
            } => AstExpressionData::Call {
                name,
                generics: generics.visit(adapter)?,
                args: args.visit(adapter)?,
            },
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                args,
            } => AstExpressionData::ObjectCall {
                object: object.visit(adapter)?,
                fn_name,
                generics: generics.visit(adapter)?,
                args: args.visit(adapter)?,
            },
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
            } => AstExpressionData::StaticCall {
                call_type: call_type.visit(adapter)?,
                fn_name,
                fn_generics: fn_generics.visit(adapter)?,
                args: args.visit(adapter)?,
                associated_trait,
            },
            AstExpressionData::Access { accessible, idx } => AstExpressionData::Access {
                accessible: accessible.visit(adapter)?,
                idx: idx.visit(adapter)?,
            },
            AstExpressionData::TupleAccess { accessible, idx } => AstExpressionData::TupleAccess {
                accessible: accessible.visit(adapter)?,
                idx,
            },
            AstExpressionData::ObjectAccess { object, mem_name } => {
                AstExpressionData::ObjectAccess {
                    object: object.visit(adapter)?,
                    mem_name,
                }
            }
            AstExpressionData::Allocate { object } => AstExpressionData::Allocate {
                object: object.visit(adapter)?,
            },
            AstExpressionData::Not(expr) => AstExpressionData::Not(expr.visit(adapter)?),
            AstExpressionData::Negate(expr) => AstExpressionData::Negate(expr.visit(adapter)?),
            AstExpressionData::BinOp { kind, lhs, rhs } => AstExpressionData::BinOp {
                kind,
                lhs: lhs.visit(adapter)?,
                rhs: rhs.visit(adapter)?,
            },
        };

        adapter.exit_expression(AstExpression { data, ty, span })
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
            | i @ AstType::SelfType
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
            t => unimplemented!("{:?}", t),
        };

        adapter.exit_type(ty)
    }
}

impl<T: Adapter> Visit<T> for AstObject {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstObject {
            name_span,
            generics,
            name,
            members,
            restrictions,
        } = adapter.enter_object(self)?;

        let i = AstObject {
            name_span,
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
            variables,
        } = adapter.enter_object_function(self)?;

        let i = AstObjectFunction {
            signature: signature.visit(adapter)?,
            definition: definition.visit(adapter)?,
            variables: variables.visit(adapter)?,
        };

        adapter.exit_object_function(i)
    }
}

impl<T: Adapter> Visit<T> for AstObjectFnSignature {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstObjectFnSignature {
            name_span,
            name,
            generics,
            has_self,
            parameter_list,
            return_type,
            restrictions,
        } = adapter.enter_object_fn_signature(self)?;

        let i = AstObjectFnSignature {
            name_span,
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
        let AstObjectMember {
            span,
            name,
            member_type,
        } = adapter.enter_object_member(self)?;

        let i = AstObjectMember {
            span,
            name,
            member_type: member_type.visit(adapter)?,
        };

        adapter.exit_object_member(i)
    }
}

impl<T: Adapter> Visit<T> for AstTrait {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstTrait {
            name_span,
            name,
            generics,
            functions,
            restrictions,
        } = adapter.enter_trait(self)?;

        let i = AstTrait {
            name_span,
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
            name_span,
            generics,
            trait_ty,
            impl_ty,
            fns,
            restrictions,
        } = adapter.enter_impl(self)?;

        let i = AstImpl {
            name_span,
            generics,
            trait_ty: trait_ty.visit(adapter)?,
            impl_ty: impl_ty.visit(adapter)?,
            fns: fns.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_impl(i)
    }
}
