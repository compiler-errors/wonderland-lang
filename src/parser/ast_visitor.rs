use crate::parser::ast::*;
use crate::util::result::*;
use std::collections::HashMap;
use std::hash::Hash;

pub trait Visit<A>: Sized {
    fn visit(self, adapter: &mut A) -> PResult<Self>;
}

pub trait Adapter {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
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
    fn enter_associated_type(&mut self, t: AstAssociatedType) -> PResult<AstAssociatedType> {
        Ok(t)
    }
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(t)
    }
    fn enter_trait_type(&mut self, t: AstTraitType) -> PResult<AstTraitType> {
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
    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        Ok(t)
    }
    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        Ok(i)
    }

    fn exit_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
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
    fn exit_associated_type(&mut self, t: AstAssociatedType) -> PResult<AstAssociatedType> {
        Ok(t)
    }
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(t)
    }
    fn exit_trait_type(&mut self, t: AstTraitType) -> PResult<AstTraitType> {
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

impl<T: Adapter, S: Visit<T>> Visit<T> for Option<S> {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        Ok(if let Some(s) = self {
            Some(s.visit(adapter)?)
        } else {
            None
        })
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
            functions,
        } = self;

        let objects = objects.visit(adapter)?;
        let traits = traits.visit(adapter)?;
        let impls = impls.visit(adapter)?;
        let functions = functions.visit(adapter)?;

        Ok(ParsedFile {
            objects,
            traits,
            impls,
            functions,
        })
    }
}

impl<T: Adapter> Visit<T> for AstFunction {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstFunction {
            name_span,
            name,
            generics,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables,
        } = adapter.enter_function(self)?;

        let i = AstFunction {
            name_span,
            name,
            generics,
            parameter_list: parameter_list.visit(adapter)?,
            return_type: return_type.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
            definition: definition.visit(adapter)?,
            variables: variables.visit(adapter)?,
        };

        adapter.exit_function(i)
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
            i @ AstStatement::Break | i @ AstStatement::Continue => i,
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
            i @ AstExpressionData::True
            | i @ AstExpressionData::False
            | i @ AstExpressionData::Null
            | i @ AstExpressionData::SelfRef
            | i @ AstExpressionData::String { .. }
            | i @ AstExpressionData::Int(..)
            | i @ AstExpressionData::Char(..)
            | i @ AstExpressionData::Identifier { .. }
            | i @ AstExpressionData::Unimplemented => i,
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
            i @ AstType::Infer(..)
            | i @ AstType::Int
            | i @ AstType::Char
            | i @ AstType::Bool
            | i @ AstType::String
            | i @ AstType::SelfType
            | i @ AstType::Generic(..)
            | i @ AstType::GenericPlaceholder(..) => i,
            AstType::Array { ty } => AstType::Array {
                ty: ty.visit(adapter)?,
            },
            AstType::Tuple { types } => AstType::Tuple {
                types: types.visit(adapter)?,
            },
            AstType::Object(name, types) => AstType::Object(name, types.visit(adapter)?),
            AstType::AssociatedType {
                obj_ty,
                trait_ty,
                name,
            } => AstType::AssociatedType {
                obj_ty: obj_ty.visit(adapter)?,
                trait_ty: trait_ty.visit(adapter)?,
                name,
            },
        };

        adapter.exit_type(ty)
    }
}

impl<T: Adapter> Visit<T> for AstTraitType {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstTraitType(name, tys) = adapter.enter_trait_type(self)?;
        let tys = tys.visit(adapter)?;

        adapter.exit_trait_type(AstTraitType(name, tys))
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
            name_span,
            name,
            generics,
            has_self,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables,
        } = adapter.enter_object_function(self)?;

        let i = AstObjectFunction {
            name_span,
            name,
            generics,
            has_self,
            parameter_list: parameter_list.visit(adapter)?,
            return_type: return_type.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
            definition: definition.visit(adapter)?,
            variables: variables.visit(adapter)?,
        };

        adapter.exit_object_function(i)
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
            associated_types,
        } = adapter.enter_trait(self)?;

        let i = AstTrait {
            name_span,
            name,
            generics,
            functions: functions.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
            associated_types: associated_types.visit(adapter)?,
        };

        adapter.exit_trait(i)
    }
}

impl<T: Adapter> Visit<T> for AstAssociatedType {
    fn visit(self, adapter: &mut T) -> PResult<AstAssociatedType> {
        let AstAssociatedType { name, restrictions } = adapter.enter_associated_type(self)?;

        let i = AstAssociatedType {
            name,
            restrictions: restrictions.visit(adapter)?,
        };

        adapter.exit_associated_type(i)
    }
}

impl<T: Adapter> Visit<T> for AstImpl {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        let AstImpl {
            impl_id,
            name_span,
            generics,
            trait_ty,
            impl_ty,
            fns,
            restrictions,
            associated_types,
        } = adapter.enter_impl(self)?;

        let i = AstImpl {
            impl_id,
            name_span,
            generics,
            trait_ty: trait_ty.visit(adapter)?,
            impl_ty: impl_ty.visit(adapter)?,
            fns: fns.visit(adapter)?,
            restrictions: restrictions.visit(adapter)?,
            associated_types: associated_types.visit(adapter)?,
        };

        adapter.exit_impl(i)
    }
}
