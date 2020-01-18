use crate::util::{FileId, FileRegistry, IntoError, PResult, Span};

use std::collections::{BTreeMap, HashMap};
use std::sync::RwLock;

#[derive(Debug, Clone)]
pub struct AstProgram {
    pub modules: Vec<AstModule>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleRef {
    Denormalized(Vec<String>),
    Normalized(FileId, String),
}

impl ModuleRef {
    pub fn full_name(&self) -> PResult<String> {
        match self {
            ModuleRef::Denormalized(path) => Ok(path.join("::")),
            ModuleRef::Normalized(file, name) => Ok(format!(
                "{}::{}",
                FileRegistry::mod_path(*file)?.join("::"),
                name
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AstUse {
    Use(Vec<String>, String),
    UseAll(Vec<String>),
}

#[derive(Debug, Clone)]
/// A file that is being parsed, along with the associated
/// parsed functions that are contained in the file.
pub struct AstModule {
    pub id: FileId,
    pub name: String,

    pub pub_uses: Vec<AstUse>,
    pub uses: Vec<AstUse>,
    pub functions: HashMap<String, AstFunction>,
    pub objects: HashMap<String, AstObject>,
    pub traits: HashMap<String, AstTrait>,
    pub enums: HashMap<String, AstEnum>,
    pub impls: HashMap<ImplId, AstImpl>,
    pub globals: HashMap<String, AstGlobalVariable>,
}

impl AstModule {
    pub fn new(
        id: FileId,
        name: String,
        pub_uses: Vec<AstUse>,
        uses: Vec<AstUse>,
        functions: HashMap<String, AstFunction>,
        objects: HashMap<String, AstObject>,
        traits: HashMap<String, AstTrait>,
        enums: HashMap<String, AstEnum>,
        impls: HashMap<ImplId, AstImpl>,
        globals: HashMap<String, AstGlobalVariable>,
    ) -> AstModule {
        AstModule {
            id,
            name,
            pub_uses,
            uses,
            functions,
            objects,
            traits,
            enums,
            impls,
            globals,
        }
    }
}

lazy_static! {
    static ref GENERIC_ID_COUNTER: RwLock<usize> = RwLock::new(1);
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstGeneric(pub GenericId, pub String);

impl AstGeneric {
    pub fn new(name: String) -> AstGeneric {
        let mut id_ref = GENERIC_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        AstGeneric(GenericId(id_ref.clone()), name)
    }
}

impl From<AstGeneric> for AstType {
    fn from(g: AstGeneric) -> Self {
        AstType::GenericPlaceholder(g.0, g.1)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstFunction {
    /// The beginning position of the function
    pub name_span: Span,

    /// The simple name of the function
    pub name: String,
    pub module_ref: ModuleRef,

    pub generics: Vec<AstGeneric>,
    /// The parameter list that the function receives
    pub parameter_list: Vec<AstNamedVariable>,
    /// The return type of the function, or AstType::None
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,

    /// The implementation, maybe.
    pub definition: Option<AstBlock>,

    /// The collection of varialbes associated with the function
    pub variables: HashMap<VariableId, AstNamedVariable>,
}

impl AstFunction {
    pub fn new(
        file: FileId,
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
        definition: Option<AstBlock>,
    ) -> AstFunction {
        AstFunction {
            name_span,
            generics,
            module_ref: ModuleRef::Normalized(file, name.clone()),
            name,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables: HashMap::new(),
        }
    }
}

lazy_static! {
    static ref VARIABLE_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone, Eq, PartialEq)]
/// A name and type associated with a parameter, along
/// with the position where this parameter is named.
pub struct AstNamedVariable {
    pub span: Span,

    pub name: String,
    pub ty: AstType,

    pub id: VariableId,
}

impl AstNamedVariable {
    pub fn new(span: Span, name: String, ty: AstType) -> AstNamedVariable {
        let mut id_ref = VARIABLE_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        AstNamedVariable {
            span,
            name: name,
            ty: ty,
            id: VariableId(id_ref.clone()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InferId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DummyId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstTraitType {
    pub name: ModuleRef,
    pub generics: Vec<AstType>,
}

impl AstTraitType {
    pub fn new(name: ModuleRef, generics: Vec<AstType>) -> AstTraitType {
        AstTraitType { name, generics }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstTraitTypeWithAssocs {
    pub trt: AstTraitType,

    /// We store these bindings like this because hashmap is not hash, I think...
    pub assoc_bindings: BTreeMap<String, AstType>,
}

impl AstTraitTypeWithAssocs {
    pub fn new(
        name: ModuleRef,
        generics: Vec<AstType>,
        assoc_bindings: BTreeMap<String, AstType>,
    ) -> AstTraitTypeWithAssocs {
        AstTraitTypeWithAssocs {
            trt: AstTraitType::new(name, generics),
            assoc_bindings,
        }
    }

    pub fn split(self) -> (AstTraitType, BTreeMap<String, AstType>) {
        let AstTraitTypeWithAssocs {
            trt: ty,
            assoc_bindings,
        } = self;
        (ty, assoc_bindings)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A type as parsed by the Parser module.
pub enum AstType {
    Infer(InferId),

    Int,
    Char,
    Bool,
    String,
    SelfType,

    Generic(String),

    Array {
        ty: Box<AstType>,
    },
    Tuple {
        types: Vec<AstType>,
    },

    ObjectEnum(ModuleRef, Vec<AstType>),
    Object(ModuleRef, Vec<AstType>),
    Enum(ModuleRef, Vec<AstType>),

    ClosureType {
        args: Vec<AstType>,
        ret_ty: Box<AstType>,
    },
    FnPointerType {
        args: Vec<AstType>,
        ret_ty: Box<AstType>,
    },

    AssociatedType {
        obj_ty: Box<AstType>,
        trait_ty: Option<AstTraitTypeWithAssocs>,
        name: String,
    },
    ElaboratedType {
        obj_ty: Box<AstType>,
        trait_ty: AstTraitTypeWithAssocs,
    },

    GenericPlaceholder(GenericId, String),

    // Generics are mapped to DummyGenerics to solidify them for type-checking.
    DummyGeneric(GenericId, String),
    Dummy(DummyId),
}

lazy_static! {
    static ref INFER_ID_COUNTER: RwLock<usize> = RwLock::new(0);
    static ref DUMMY_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}

impl AstType {
    pub fn infer() -> AstType {
        let mut id_ref = INFER_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        AstType::Infer(InferId(id_ref.clone()))
    }

    pub fn dummy() -> AstType {
        let mut id_ref = DUMMY_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        AstType::Dummy(DummyId(id_ref.clone()))
    }

    pub fn array(ty: AstType) -> AstType {
        AstType::Array { ty: Box::new(ty) }
    }

    pub fn get_element(ty: &AstType) -> PResult<AstType> {
        if let AstType::Array { ty } = ty {
            Ok(*ty.clone())
        } else {
            PResult::error(format!(
                "Trying to access element type of non-array: {:?}",
                ty
            ))
        }
    }

    pub fn tuple(types: Vec<AstType>) -> AstType {
        AstType::Tuple { types }
    }

    pub fn none() -> AstType {
        AstType::Tuple { types: Vec::new() }
    }

    pub fn object(object: ModuleRef, generics: Vec<AstType>) -> AstType {
        AstType::Object(object, generics)
    }

    pub fn enumerable(enumerable: ModuleRef, generics: Vec<AstType>) -> AstType {
        AstType::Enum(enumerable, generics)
    }

    pub fn closure_type(args: Vec<AstType>, ret_ty: AstType) -> AstType {
        AstType::ClosureType {
            args,
            ret_ty: Box::new(ret_ty),
        }
    }

    pub fn fn_ptr_type(args: Vec<AstType>, ret_ty: AstType) -> AstType {
        AstType::FnPointerType {
            args,
            ret_ty: Box::new(ret_ty),
        }
    }

    pub fn generic(generic: String) -> AstType {
        AstType::Generic(generic)
    }

    pub fn associated_type(ty: AstType, name: String) -> AstType {
        AstType::AssociatedType {
            obj_ty: Box::new(ty),
            trait_ty: None,
            name,
        }
    }

    pub fn elaborated_type(ty: AstType, trt: AstTraitTypeWithAssocs) -> AstType {
        AstType::ElaboratedType {
            obj_ty: Box::new(ty),
            trait_ty: trt,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
/// A collection of statements, given by a `{}` block.
pub struct AstBlock {
    pub statements: Vec<AstStatement>,
    pub expression: Box<AstExpression>,
}

impl AstBlock {
    pub fn new(statements: Vec<AstStatement>, expr: AstExpression) -> AstBlock {
        AstBlock {
            statements,
            expression: Box::new(expr),
        }
    }

    pub fn empty(span: Span) -> AstBlock {
        AstBlock {
            statements: Vec::new(),
            expression: Box::new(AstExpression::nothing(span)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LoopId(pub usize);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstStatement {
    Let {
        pattern: AstMatchPattern,
        value: AstExpression,
    },
    Break {
        label: Option<String>,
        id: Option<LoopId>,
        value: AstExpression,
    },
    Continue {
        label: Option<String>,
        id: Option<LoopId>,
    },
    Return {
        value: AstExpression,
    },
    Assert {
        condition: AstExpression,
    },
    Expression {
        expression: AstExpression,
    },
}

lazy_static! {
    static ref LOOP_ID_COUNTER: RwLock<usize> = RwLock::new(1);
}

impl AstStatement {
    pub fn let_statement(pattern: AstMatchPattern, value: AstExpression) -> AstStatement {
        AstStatement::Let { pattern, value }
    }

    pub fn return_statement(value: AstExpression) -> AstStatement {
        AstStatement::Return { value }
    }

    pub fn return_nothing(span: Span) -> AstStatement {
        AstStatement::Return {
            value: AstExpression::nothing(span),
        }
    }

    pub fn break_stmt(value: AstExpression, label: Option<String>) -> AstStatement {
        AstStatement::Break {
            value,
            label,
            id: None,
        }
    }

    pub fn continue_stmt(label: Option<String>) -> AstStatement {
        AstStatement::Continue { label, id: None }
    }

    pub fn assert_statement(condition: AstExpression) -> AstStatement {
        AstStatement::Assert { condition }
    }

    pub fn expression_statement(expression: AstExpression) -> AstStatement {
        AstStatement::Expression { expression }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstExpression {
    pub data: AstExpressionData,
    pub ty: AstType,
    pub span: Span,
}

type SubExpression = Box<AstExpression>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstExpressionData {
    Unimplemented,
    SelfRef,
    Literal(AstLiteral),
    Identifier {
        name: String,
        variable_id: Option<VariableId>,
    },
    GlobalVariable {
        name: ModuleRef,
    },
    GlobalFn {
        name: ModuleRef,
    },
    Tuple {
        values: Vec<AstExpression>,
    },
    ArrayLiteral {
        elements: Vec<AstExpression>,
    },
    Closure {
        params: Vec<AstNamedVariable>,
        expr: SubExpression,

        captured: Option<Vec<(AstNamedVariable, AstNamedVariable)>>,
        variables: Option<HashMap<VariableId, AstNamedVariable>>,
    },

    /// A regular function call
    FnCall {
        fn_name: ModuleRef,
        generics: Vec<AstType>,
        args: Vec<AstExpression>,
    },
    // A call to an subexpression.
    ExprCall {
        expr: SubExpression,
        args: Vec<AstExpression>,
    },
    /// Call an object's member function
    ObjectCall {
        object: SubExpression,
        fn_name: String,
        generics: Vec<AstType>,
        args: Vec<AstExpression>,
    },
    /// Call an object's static function
    StaticCall {
        call_type: AstType,
        fn_name: String,
        fn_generics: Vec<AstType>,
        args: Vec<AstExpression>,

        associated_trait: Option<AstTraitTypeWithAssocs>,
        impl_signature: Option<AstImplSignature>,
    },
    /// An array access `a[1u]`
    ArrayAccess {
        accessible: SubExpression,
        idx: SubExpression,
    },
    /// A tuple access `a:1`
    TupleAccess {
        accessible: SubExpression,
        idx: usize,
    },
    /// Call an object's member
    ObjectAccess {
        object: SubExpression,
        mem_name: String,
        mem_idx: Option<usize>,
    },

    AllocateObject {
        object: ModuleRef,
        generics: Vec<AstType>,
        children: HashMap<String, AstExpression>,
        children_idxes: Option<HashMap<String, usize>>,
    },

    AllocateArray {
        object: AstType,
        size: SubExpression,
    },
    Not(SubExpression),
    Negate(SubExpression),

    Assign {
        lhs: SubExpression,
        rhs: SubExpression,
    },

    BinOp {
        kind: BinOpKind,
        lhs: SubExpression,
        rhs: SubExpression,
    },

    Block {
        block: AstBlock,
    },
    If {
        condition: SubExpression,
        block: AstBlock,
        else_block: AstBlock,
    },
    Match {
        expression: SubExpression,
        branches: Vec<AstMatchBranch>,
    },
    While {
        label: Option<String>,
        id: LoopId,
        condition: SubExpression,
        block: AstBlock,
        else_block: AstBlock,
    },
    For {
        label: Option<String>,
        pattern: AstMatchPattern,
        iterable: SubExpression,
        block: AstBlock,
        else_block: AstBlock,
    },

    PositionalEnum {
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: Vec<AstExpression>,
    },
    NamedEnum {
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: HashMap<String, AstExpression>,
    },
    PlainEnum {
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
    },
    As {
        expression: SubExpression,
        ty: AstType,
    },
    Instruction {
        instruction: String,
        arguments: Vec<InstructionArgument>,
        output: InstructionOutput,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InstructionArgument {
    Expression(AstExpression),
    Type(AstType),
    Anonymous(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InstructionOutput {
    Type(AstType),
    Anonymous(String),
}

impl AstExpression {
    pub fn block(span: Span, block: AstBlock) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Block { block },
            ty: AstType::infer(),
        }
    }

    pub fn if_statement(
        span: Span,
        condition: AstExpression,
        block: AstBlock,
        else_block: AstBlock,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::If {
                condition: Box::new(condition),
                block,
                else_block,
            },
            ty: AstType::infer(),
        }
    }

    pub fn match_statement(
        span: Span,
        expression: AstExpression,
        branches: Vec<AstMatchBranch>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Match {
                expression: Box::new(expression),
                branches,
            },
            ty: AstType::infer(),
        }
    }

    pub fn while_loop(
        span: Span,
        label: Option<String>,
        condition: AstExpression,
        block: AstBlock,
        else_block: AstBlock,
    ) -> AstExpression {
        let mut id_ref = LOOP_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        AstExpression {
            span,
            data: AstExpressionData::While {
                id: LoopId(id_ref.clone()),
                label,
                condition: Box::new(condition),
                block,
                else_block,
            },
            ty: AstType::infer(),
        }
    }

    pub fn for_loop(
        span: Span,
        label: Option<String>,
        pattern: AstMatchPattern,
        iterable: AstExpression,
        block: AstBlock,
        else_block: AstBlock,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::For {
                label,
                pattern,
                iterable: Box::new(iterable),
                block,
                else_block,
            },
            ty: AstType::infer(),
        }
    }

    pub fn positional_enum_constructor(
        span: Span,
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: Vec<AstExpression>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
            },
            ty: AstType::infer(),
        }
    }

    pub fn named_enum_constructor(
        span: Span,
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: HashMap<String, AstExpression>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
            },
            ty: AstType::infer(),
        }
    }

    pub fn plain_enum_constructor(
        span: Span,
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::PlainEnum {
                enumerable,
                generics,
                variant,
            },
            ty: AstType::infer(),
        }
    }

    pub fn literal(span: Span, lit: AstLiteral) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Literal(lit),
            ty: AstType::infer(),
        }
    }

    pub fn identifier(span: Span, identifier: String) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Identifier {
                name: identifier,
                variable_id: None,
            },
            ty: AstType::infer(),
        }
    }

    pub fn global_variable(span: Span, path: Vec<String>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::GlobalVariable {
                name: ModuleRef::Denormalized(path),
            },
            ty: AstType::infer(),
        }
    }

    pub fn global_fn(span: Span, name: ModuleRef) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::GlobalFn { name },
            ty: AstType::infer(),
        }
    }

    pub fn tuple_literal(span: Span, values: Vec<AstExpression>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Tuple { values },
            ty: AstType::infer(),
        }
    }

    pub fn empty_array_literal(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ArrayLiteral {
                elements: Vec::new(),
            },
            ty: AstType::infer(),
        }
    }

    pub fn array_literal(span: Span, elements: Vec<AstExpression>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ArrayLiteral { elements },
            ty: AstType::infer(),
        }
    }

    pub fn closure(
        span: Span,
        params: Vec<AstNamedVariable>,
        expr: AstExpression,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Closure {
                params,
                expr: Box::new(expr),
                captured: None,
                variables: None,
            },
            ty: AstType::infer(),
        }
    }

    pub fn call(
        span: Span,
        fn_name: ModuleRef,
        generics: Vec<AstType>,
        args: Vec<AstExpression>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            },
            ty: AstType::infer(),
        }
    }

    pub fn expr_call(span: Span, expr: AstExpression, args: Vec<AstExpression>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ExprCall {
                expr: Box::new(expr),
                args,
            },
            ty: AstType::infer(),
        }
    }

    pub fn object_call(
        span: Span,
        object: AstExpression,
        fn_name: String,
        generics: Vec<AstType>,
        args: Vec<AstExpression>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ObjectCall {
                object: Box::new(object),
                fn_name: fn_name,
                generics: generics,
                args: args,
            },
            ty: AstType::infer(),
        }
    }

    pub fn static_call(
        span: Span,
        call_type: AstType,
        fn_name: String,
        fn_generics: Vec<AstType>,
        args: Vec<AstExpression>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait: None,
                impl_signature: None,
            },
            ty: AstType::infer(),
        }
    }

    pub fn unimplemented(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Unimplemented,
            ty: AstType::infer(),
        }
    }

    pub fn access(span: Span, lhs: AstExpression, idx: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ArrayAccess {
                accessible: Box::new(lhs),
                idx: Box::new(idx),
            },
            ty: AstType::infer(),
        }
    }

    pub fn tuple_access(span: Span, lhs: AstExpression, idx: usize) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::TupleAccess {
                accessible: Box::new(lhs),
                idx: idx,
            },
            ty: AstType::infer(),
        }
    }

    pub fn object_access(span: Span, object: AstExpression, mem_name: String) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ObjectAccess {
                object: Box::new(object),
                mem_name,
                mem_idx: None,
            },
            ty: AstType::infer(),
        }
    }

    pub fn allocate_object(
        span: Span,
        object: ModuleRef,
        generics: Vec<AstType>,
        children: HashMap<String, AstExpression>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::AllocateObject {
                object,
                generics,
                children,
                children_idxes: None,
            },
            ty: AstType::infer(),
        }
    }

    pub fn allocate_array(span: Span, object: AstType, size: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::AllocateArray {
                object,
                size: Box::new(size),
            },
            ty: AstType::infer(),
        }
    }

    pub fn assign(span: Span, lhs: AstExpression, rhs: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Assign {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            ty: AstType::infer(),
        }
    }

    pub fn binop(
        span: Span,
        lhs: AstExpression,
        rhs: AstExpression,
        binop: BinOpKind,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::BinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind: binop,
            },
            ty: AstType::infer(),
        }
    }

    pub fn not(span: Span, lhs: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Not(Box::new(lhs)),
            ty: AstType::infer(),
        }
    }

    pub fn neg(span: Span, lhs: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Negate(Box::new(lhs)),
            ty: AstType::infer(),
        }
    }

    pub fn nothing(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Tuple { values: Vec::new() },
            ty: AstType::infer(),
        }
    }

    pub fn self_ref(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::SelfRef,
            ty: AstType::infer(),
        }
    }

    pub fn transmute(span: Span, expr: AstExpression, ty: AstType) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::As {
                expression: Box::new(expr),
                ty,
            },
            ty: AstType::infer(),
        }
    }

    pub fn instruction(
        span: Span,
        instruction: String,
        arguments: Vec<InstructionArgument>,
        output: InstructionOutput,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Instruction {
                instruction,
                arguments,
                output,
            },
            ty: AstType::infer(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstMatchBranch {
    pub pattern: AstMatchPattern,
    pub expression: AstExpression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstMatchPattern {
    pub data: AstMatchPatternData,
    pub ty: AstType,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstMatchPatternData {
    Underscore,
    Identifier(AstNamedVariable),
    PositionalEnum {
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: Vec<AstMatchPattern>,
        ignore_rest: bool,
    },
    NamedEnum {
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: HashMap<String, AstMatchPattern>,
        ignore_rest: bool,
    },
    PlainEnum {
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
    },
    Tuple(Vec<AstMatchPattern>),
    Literal(AstLiteral),
}

impl AstMatchPattern {
    pub fn underscore() -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Underscore,
            ty: AstType::infer(),
        }
    }

    pub fn identifier(name_span: Span, name: String, ty: AstType) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Identifier(AstNamedVariable::new(
                name_span,
                name,
                ty.clone(),
            )),
            ty,
        }
    }

    pub fn positional_enum(
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: Vec<AstMatchPattern>,
        ignore_rest: bool,
    ) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            },
            ty: AstType::infer(),
        }
    }

    pub fn named_enum(
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: HashMap<String, AstMatchPattern>,
        ignore_rest: bool,
    ) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            },
            ty: AstType::infer(),
        }
    }

    pub fn plain_enum(
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
    ) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::PlainEnum {
                enumerable,
                generics,
                variant,
            },
            ty: AstType::infer(),
        }
    }

    pub fn tuple(children: Vec<AstMatchPattern>) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Tuple(children),
            ty: AstType::infer(),
        }
    }

    pub fn literal(lit: AstLiteral) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Literal(lit),
            ty: AstType::infer(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstLiteral {
    True,
    False,
    String(String),
    Int(String),
    Char(char),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// The kind of binary operation
pub enum BinOpKind {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    EqualsEquals,
    NotEqual,
    And,
    Or,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstObject {
    pub name_span: Span,
    pub module_ref: ModuleRef,
    /// The object name
    pub name: String,

    /// The beginning position of the object
    pub generics: Vec<AstGeneric>,
    /// The members that are contained in the object
    pub members: Vec<AstObjectMember>,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstObject {
    pub fn new(
        file: FileId,
        name_span: Span,
        generics: Vec<AstGeneric>,
        name: String,
        members: Vec<AstObjectMember>,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstObject {
        AstObject {
            name_span,
            generics,
            module_ref: ModuleRef::Normalized(file, name.clone()),
            name,
            restrictions,
            members,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstObjectFunction {
    pub name_span: Span,

    /// The beginning position of the function
    /// The simple name of the function
    pub name: String,

    pub generics: Vec<AstGeneric>,
    pub restrictions: Vec<AstTypeRestriction>,

    /// Whether the function is a member or static function of the type
    pub has_self: bool,
    /// The parameter list that the function receives
    pub parameter_list: Vec<AstNamedVariable>,
    /// The return type of the function, or AstType::None
    pub return_type: AstType,

    /// The collection of statements associated with the function
    pub definition: Option<AstBlock>,

    /// Used during analysis...
    pub variables: HashMap<VariableId, AstNamedVariable>,
}

impl AstObjectFunction {
    pub fn new(
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        has_self: bool,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
        definition: Option<AstBlock>,
    ) -> AstObjectFunction {
        AstObjectFunction {
            name_span,
            name,
            generics,
            has_self,
            parameter_list,
            return_type,
            restrictions,
            definition,
            variables: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstObjectMember {
    pub span: Span,
    pub name: String,
    pub member_type: AstType,
}

impl AstObjectMember {
    pub fn new(span: Span, name: String, member_type: AstType) -> AstObjectMember {
        AstObjectMember {
            span,
            name,
            member_type,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstTrait {
    pub name_span: Span,

    pub name: String,
    pub module_ref: ModuleRef,

    pub generics: Vec<AstGeneric>,
    pub restrictions: Vec<AstTypeRestriction>,

    pub functions: HashMap<String, AstObjectFunction>,
    pub associated_types: HashMap<String, AstAssociatedType>,
}

impl AstTrait {
    pub fn new(
        file: FileId,
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        functions: HashMap<String, AstObjectFunction>,
        restrictions: Vec<AstTypeRestriction>,
        associated_types: HashMap<String, AstAssociatedType>,
    ) -> AstTrait {
        AstTrait {
            name_span,
            module_ref: ModuleRef::Normalized(file, name.clone()),
            name,
            generics,
            functions,
            restrictions,
            associated_types,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstAssociatedType {
    pub name: String,
    pub restrictions: Vec<AstTraitTypeWithAssocs>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstTypeRestriction {
    pub ty: AstType,
    pub trt: AstTraitTypeWithAssocs,
}

impl AstTypeRestriction {
    pub fn new(ty: AstType, trt: AstTraitTypeWithAssocs) -> AstTypeRestriction {
        AstTypeRestriction { ty, trt }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImplId(pub usize);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstImpl {
    pub impl_id: ImplId,
    pub name_span: Span,

    pub generics: Vec<AstGeneric>,
    pub restrictions: Vec<AstTypeRestriction>,

    pub trait_ty: AstTraitType,
    pub impl_ty: AstType,

    pub fns: HashMap<String, AstObjectFunction>,
    pub associated_types: HashMap<String, AstType>,
}

lazy_static! {
    static ref IMPL_ID_COUNTER: RwLock<usize> = RwLock::new(1);
    static ref IMPL_ID_DUMMY: ImplId = ImplId(0);
}

impl AstImpl {
    pub fn new(
        name_span: Span,
        generics: Vec<AstGeneric>,
        trait_ty: AstTraitType,
        impl_ty: AstType,
        fns: HashMap<String, AstObjectFunction>,
        restrictions: Vec<AstTypeRestriction>,
        associated_types: HashMap<String, AstType>,
    ) -> AstImpl {
        let mut id_ref = IMPL_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        AstImpl {
            impl_id: ImplId(id_ref.clone()),
            name_span,
            generics,
            trait_ty,
            impl_ty,
            restrictions,
            fns,
            associated_types,
        }
    }

    pub fn new_id() -> ImplId {
        let mut id_ref = IMPL_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        ImplId(id_ref.clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstGlobalVariable {
    pub name_span: Span,

    pub name: String,
    pub module_ref: ModuleRef,

    pub ty: AstType,
    pub init: AstExpression,
}

impl AstGlobalVariable {
    pub fn new(
        file_id: FileId,
        name_span: Span,
        name: String,
        ty: AstType,
        init: AstExpression,
    ) -> AstGlobalVariable {
        AstGlobalVariable {
            name_span,
            module_ref: ModuleRef::Normalized(file_id, name.clone()),
            name,
            ty,
            init,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstEnum {
    pub name_span: Span,

    pub name: String,
    pub module_ref: ModuleRef,

    pub generics: Vec<AstGeneric>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub variants: HashMap<String, AstEnumVariant>,
}

impl AstEnum {
    pub fn new(
        file_id: FileId,
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        restrictions: Vec<AstTypeRestriction>,
        variants: HashMap<String, AstEnumVariant>,
    ) -> AstEnum {
        AstEnum {
            name_span,
            module_ref: ModuleRef::Normalized(file_id, name.clone()),
            name,
            generics,
            restrictions,
            variants,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstEnumVariant {
    pub name_span: Span,
    pub name: String,

    pub fields: Vec<AstType>,

    /// If Some, then this enum is structural and this maps field names to positions.
    /// If None, then this enum is positional, and we cannot use field names.
    pub field_names: Option<HashMap<String, usize>>,
}

/// Used in tyck
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstImplSignature {
    pub impl_id: ImplId,
    pub generics: Vec<AstType>,
}
