use crate::{parser::ast::*, util::PResult};

pub trait AstAdapter {
    fn enter_ast_program(&mut self, p: AstProgram) -> PResult<AstProgram> {
        Ok(p)
    }

    fn enter_ast_module(&mut self, m: AstModule) -> PResult<AstModule> {
        Ok(m)
    }
    fn enter_module_ref(&mut self, m: ModuleRef) -> PResult<ModuleRef> {
        Ok(m)
    }
    fn enter_ast_use(&mut self, u: AstUse) -> PResult<AstUse> {
        Ok(u)
    }

    fn enter_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        Ok(f)
    }
    fn enter_ast_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        Ok(b)
    }
    fn enter_ast_named_variable(&mut self, p: AstNamedVariable) -> PResult<AstNamedVariable> {
        Ok(p)
    }
    fn enter_ast_type_restriction(&mut self, t: AstTypeRestriction) -> PResult<AstTypeRestriction> {
        Ok(t)
    }
    fn enter_ast_associated_type(&mut self, t: AstAssociatedType) -> PResult<AstAssociatedType> {
        Ok(t)
    }
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(t)
    }
    fn enter_ast_trait_type(&mut self, t: AstTraitType) -> PResult<AstTraitType> {
        Ok(t)
    }
    fn enter_ast_trait_type_with_assocs(
        &mut self,
        t: AstTraitTypeWithAssocs,
    ) -> PResult<AstTraitTypeWithAssocs> {
        Ok(t)
    }
    fn enter_ast_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        Ok(s)
    }
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        Ok(e)
    }
    fn enter_ast_literal(&mut self, e: AstLiteral) -> PResult<AstLiteral> {
        Ok(e)
    }
    fn enter_ast_match_branch(&mut self, b: AstMatchBranch) -> PResult<AstMatchBranch> {
        Ok(b)
    }
    fn enter_ast_match_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        Ok(p)
    }
    fn enter_ast_object(&mut self, o: AstObject) -> PResult<AstObject> {
        Ok(o)
    }
    fn enter_ast_object_member(&mut self, o: AstObjectMember) -> PResult<AstObjectMember> {
        Ok(o)
    }
    fn enter_ast_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        Ok(o)
    }
    fn enter_ast_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        Ok(t)
    }
    fn enter_ast_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        Ok(e)
    }
    fn enter_ast_enum_variant(&mut self, e: AstEnumVariant) -> PResult<AstEnumVariant> {
        Ok(e)
    }
    fn enter_ast_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        Ok(i)
    }
    fn enter_ast_impl_signature(&mut self, i: AstImplSignature) -> PResult<AstImplSignature> {
        Ok(i)
    }
    fn enter_ast_global_variable(&mut self, g: AstGlobalVariable) -> PResult<AstGlobalVariable> {
        Ok(g)
    }

    fn exit_ast_program(&mut self, p: AstProgram) -> PResult<AstProgram> {
        Ok(p)
    }

    fn exit_ast_module(&mut self, m: AstModule) -> PResult<AstModule> {
        Ok(m)
    }
    fn exit_module_ref(&mut self, m: ModuleRef) -> PResult<ModuleRef> {
        Ok(m)
    }
    fn exit_ast_use(&mut self, u: AstUse) -> PResult<AstUse> {
        Ok(u)
    }

    fn exit_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        Ok(f)
    }
    fn exit_ast_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        Ok(b)
    }
    fn exit_ast_named_variable(&mut self, p: AstNamedVariable) -> PResult<AstNamedVariable> {
        Ok(p)
    }
    fn exit_ast_type_restriction(&mut self, t: AstTypeRestriction) -> PResult<AstTypeRestriction> {
        Ok(t)
    }
    fn exit_ast_associated_type(&mut self, t: AstAssociatedType) -> PResult<AstAssociatedType> {
        Ok(t)
    }
    fn exit_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(t)
    }
    fn exit_ast_trait_type(&mut self, t: AstTraitType) -> PResult<AstTraitType> {
        Ok(t)
    }
    fn exit_ast_trait_type_with_assocs(
        &mut self,
        t: AstTraitTypeWithAssocs,
    ) -> PResult<AstTraitTypeWithAssocs> {
        Ok(t)
    }
    fn exit_ast_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        Ok(s)
    }
    fn exit_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        Ok(e)
    }
    fn exit_ast_literal(&mut self, e: AstLiteral) -> PResult<AstLiteral> {
        Ok(e)
    }
    fn exit_ast_match_branch(&mut self, b: AstMatchBranch) -> PResult<AstMatchBranch> {
        Ok(b)
    }
    fn exit_ast_match_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        Ok(p)
    }
    fn exit_ast_object(&mut self, o: AstObject) -> PResult<AstObject> {
        Ok(o)
    }
    fn exit_ast_object_member(&mut self, o: AstObjectMember) -> PResult<AstObjectMember> {
        Ok(o)
    }
    fn exit_ast_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        Ok(o)
    }
    fn exit_ast_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        Ok(t)
    }
    fn exit_ast_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        Ok(e)
    }
    fn exit_ast_enum_variant(&mut self, e: AstEnumVariant) -> PResult<AstEnumVariant> {
        Ok(e)
    }
    fn exit_ast_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        Ok(i)
    }
    fn exit_ast_impl_signature(&mut self, i: AstImplSignature) -> PResult<AstImplSignature> {
        Ok(i)
    }
    fn exit_ast_global_variable(&mut self, g: AstGlobalVariable) -> PResult<AstGlobalVariable> {
        Ok(g)
    }
}
