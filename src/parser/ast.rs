use crate::util::Span;
use std::collections::HashMap;
use std::sync::RwLock;

#[derive(Debug)]
/// A file that is being parsed, along with the associated
/// parsed functions that are contained in the file.
pub struct ParsedFile {
    pub functions: Vec<AstFunction>,
    pub objects: Vec<AstObject>,
    pub traits: Vec<AstTrait>,
    pub impls: Vec<AstImpl>,
}

impl ParsedFile {
    pub fn new(
        functions: Vec<AstFunction>,
        objects: Vec<AstObject>,
        traits: Vec<AstTrait>,
        impls: Vec<AstImpl>,
    ) -> ParsedFile {
        ParsedFile {
            functions: functions,
            objects: objects,
            traits: traits,
            impls: impls,
        }
    }
}

#[derive(Debug)]
pub struct AstFunction {
    pub name_span: Span,
    /// The beginning position of the function
    /// The simple name of the function
    pub name: String,
    pub generics: Vec<String>,
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
        name_span: Span,
        name: String,
        generics: Vec<String>,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
        definition: Option<AstBlock>,
    ) -> AstFunction {
        AstFunction {
            name_span,
            generics: generics,
            name: name,
            parameter_list: parameter_list,
            return_type: return_type,
            restrictions: restrictions,
            definition,
            variables: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone)]
/// A name and type associated with a parameter, along
/// with the position where this parameter is named.
pub struct AstNamedVariable {
    pub span: Span,

    pub name: String,
    pub ty: AstType,

    /// Used in analyzer. Not populated before this.
    pub id: Option<VariableId>,
}

impl AstNamedVariable {
    pub fn new(span: Span, name: String, ty: AstType) -> AstNamedVariable {
        AstNamedVariable {
            span,
            name: name,
            ty: ty,
            id: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InferId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstTraitType(pub String, pub Vec<AstType>);

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
    Object(String, Vec<AstType>),
    AssociatedType {
        obj_ty: Box<AstType>,
        trait_ty: Option<AstTraitType>,
        name: String,
    },

    GenericPlaceholder(GenericId, String),
}

lazy_static! {
    static ref INFER_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}

impl AstType {
    pub fn infer() -> AstType {
        let mut id_ref = INFER_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        AstType::Infer(InferId(id_ref.clone()))
    }

    pub fn array(ty: AstType) -> AstType {
        AstType::Array { ty: Box::new(ty) }
    }

    pub fn tuple(types: Vec<AstType>) -> AstType {
        AstType::Tuple { types }
    }

    pub fn none() -> AstType {
        AstType::Tuple { types: Vec::new() }
    }

    pub fn object(obj: String, generics: Vec<AstType>) -> AstType {
        AstType::Object(obj, generics)
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
}

#[derive(Debug)]
/// A collection of statements, given by a `{}` block.
pub struct AstBlock {
    pub statements: Vec<AstStatement>,
}

impl AstBlock {
    pub fn new(statements: Vec<AstStatement>) -> AstBlock {
        AstBlock { statements }
    }

    pub fn empty() -> AstBlock {
        AstBlock {
            statements: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum AstStatement {
    Block {
        block: AstBlock,
    },
    Let {
        name_span: Span,
        var_name: String,
        ty: AstType,
        value: AstExpression,
    },
    If {
        condition: AstExpression,
        block: AstBlock,
        else_block: AstBlock,
    },
    While {
        condition: AstExpression,
        block: AstBlock,
    },
    Break,
    Continue,
    Return {
        value: AstExpression,
    },
    Assert {
        condition: AstExpression,
    },
    Expression {
        expression: AstExpression,
    },
    Unimplemented,
}

impl AstStatement {
    pub fn block(block: AstBlock) -> AstStatement {
        AstStatement::Block { block }
    }

    pub fn let_statement(
        name_span: Span,
        var_name: String,
        ty: AstType,
        value: AstExpression,
    ) -> AstStatement {
        AstStatement::Let {
            name_span,
            var_name,
            ty,
            value,
        }
    }

    pub fn if_statement(
        condition: AstExpression,
        block: AstBlock,
        else_block: AstBlock,
    ) -> AstStatement {
        AstStatement::If {
            condition: condition,
            block: block,
            else_block: else_block,
        }
    }

    pub fn while_loop(condition: AstExpression, block: AstBlock) -> AstStatement {
        AstStatement::While {
            condition: condition,
            block: block,
        }
    }

    pub fn return_statement(value: AstExpression) -> AstStatement {
        AstStatement::Return { value: value }
    }

    pub fn return_nothing() -> AstStatement {
        AstStatement::Return {
            value: AstExpression::nothing(Span::new(0, 0)),
        }
    }

    pub fn assert_statement(condition: AstExpression) -> AstStatement {
        AstStatement::Assert {
            condition: condition,
        }
    }

    pub fn expression_statement(expression: AstExpression) -> AstStatement {
        AstStatement::Expression {
            expression: expression,
        }
    }

    pub fn break_stmt() -> AstStatement {
        AstStatement::Break
    }

    pub fn continue_stmt() -> AstStatement {
        AstStatement::Continue
    }

    pub fn unimplemented() -> AstStatement {
        AstStatement::Unimplemented
    }
}

#[derive(Debug)]
pub struct AstExpression {
    pub data: AstExpressionData,
    pub ty: Option<AstType>,
    pub span: Span,
}

type SubExpression = Box<AstExpression>;

#[derive(Debug)]
pub enum AstExpressionData {
    Nothing,
    True,
    False,
    Null,
    SelfRef,
    String {
        string: String,
        len: usize,
    },
    Int(String),
    Char(char),
    Identifier {
        name: String,
        variable_id: Option<VariableId>,
    },
    Tuple {
        values: Vec<AstExpression>,
    },
    Array {
        elements: Vec<AstExpression>,
    },

    /// A regular function call
    Call {
        name: String,
        generics: Vec<AstType>,
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
        associated_trait: Option<AstTraitType>,
    },
    /// An array access `a[1u]`
    Access {
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
    },

    Allocate {
        object: AstType,
    },

    Not(SubExpression),
    Negate(SubExpression),

    BinOp {
        kind: BinOpKind,
        lhs: SubExpression,
        rhs: SubExpression,
    },
}

#[derive(Clone, Copy, Debug)]
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
    Set,
}

impl AstExpression {
    pub fn string_literal(span: Span, string: String, len: usize) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::String { string, len },
            ty: None,
        }
    }

    pub fn char_literal(span: Span, ch: char) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Char(ch),
            ty: None,
        }
    }

    pub fn int_literal(span: Span, num: String) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Int(num),
            ty: None,
        }
    }

    pub fn identifier(span: Span, identifier: String) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Identifier {
                name: identifier,
                variable_id: None,
            },
            ty: None,
        }
    }

    pub fn tuple_literal(span: Span, values: Vec<AstExpression>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Tuple { values },
            ty: None,
        }
    }

    pub fn empty_array_literal(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Array {
                elements: Vec::new(),
            },
            ty: None,
        }
    }

    pub fn array_literal(span: Span, elements: Vec<AstExpression>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Array { elements },
            ty: None,
        }
    }

    pub fn call(
        span: Span,
        name: String,
        generics: Vec<AstType>,
        args: Vec<AstExpression>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Call {
                name,
                generics,
                args,
            },
            ty: None,
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
            ty: None,
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
                fn_name: fn_name,
                fn_generics: fn_generics,
                args: args,
                associated_trait: None,
            },
            ty: None,
        }
    }

    pub fn access(span: Span, lhs: AstExpression, idx: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Access {
                accessible: Box::new(lhs),
                idx: Box::new(idx),
            },
            ty: None,
        }
    }

    pub fn tuple_access(span: Span, lhs: AstExpression, idx: usize) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::TupleAccess {
                accessible: Box::new(lhs),
                idx: idx,
            },
            ty: None,
        }
    }

    pub fn object_access(span: Span, object: AstExpression, mem_name: String) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ObjectAccess {
                object: Box::new(object),
                mem_name,
            },
            ty: None,
        }
    }

    pub fn allocate(span: Span, object: AstType) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Allocate { object },
            ty: None,
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
            ty: None,
        }
    }

    pub fn not(span: Span, lhs: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Not(Box::new(lhs)),
            ty: None,
        }
    }

    pub fn neg(span: Span, lhs: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Negate(Box::new(lhs)),
            ty: None,
        }
    }

    pub fn nothing(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Nothing,
            ty: None,
        }
    }

    pub fn true_lit(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::True,
            ty: None,
        }
    }

    pub fn false_lit(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::False,
            ty: None,
        }
    }

    pub fn null_lit(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Null,
            ty: None,
        }
    }

    pub fn self_ref(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::SelfRef,
            ty: None,
        }
    }
}

#[derive(Debug)]
pub struct AstObject {
    pub name_span: Span,

    /// The beginning position of the object
    pub generics: Vec<String>,
    /// The object name
    pub name: String,
    /// The members that are contained in the object
    pub members: Vec<AstObjectMember>,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstObject {
    pub fn new(
        name_span: Span,
        generics: Vec<String>,
        name: String,
        members: Vec<AstObjectMember>,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstObject {
        AstObject {
            name_span,
            generics: generics,
            name: name,
            restrictions: restrictions,
            members: members,
        }
    }
}

#[derive(Debug)]
pub struct AstObjectFunction {
    pub name_span: Span,

    /// The beginning position of the function
    /// The simple name of the function
    pub name: String,
    pub generics: Vec<String>,
    /// Whether the function is a member or static function of the type
    pub has_self: bool,
    /// The parameter list that the function receives
    pub parameter_list: Vec<AstNamedVariable>,
    /// The return type of the function, or AstType::None
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,

    /// The collection of statements associated with the function
    pub definition: Option<AstBlock>,

    /// Used during analysis...
    pub variables: HashMap<VariableId, AstNamedVariable>,
}

impl AstObjectFunction {
    pub fn new(
        name_span: Span,
        name: String,
        generics: Vec<String>,
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

#[derive(Debug)]
pub struct AstObjectMember {
    pub span: Span,
    pub name: String,
    pub member_type: AstType,
}

impl AstObjectMember {
    pub fn new(span: Span, name: String, member_type: AstType) -> AstObjectMember {
        AstObjectMember {
            span,
            name: name,
            member_type: member_type,
        }
    }
}

#[derive(Debug)]
pub struct AstTrait {
    pub name_span: Span,

    pub name: String,
    pub generics: Vec<String>,
    pub functions: Vec<AstObjectFunction>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub associated_types: HashMap<String, AstAssociatedType>,
}

impl AstTrait {
    pub fn new(
        name_span: Span,
        name: String,
        generics: Vec<String>,
        functions: Vec<AstObjectFunction>,
        restrictions: Vec<AstTypeRestriction>,
        associated_types: HashMap<String, AstAssociatedType>,
    ) -> AstTrait {
        AstTrait {
            name_span,
            name,
            generics,
            functions,
            restrictions,
            associated_types,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstAssociatedType {
    pub name: String,
    pub restrictions: Vec<AstTraitType>,
}

#[derive(Debug, Clone)]
pub struct AstTypeRestriction {
    pub ty: AstType,
    pub trt: AstTraitType,
}

impl AstTypeRestriction {
    pub fn new(ty: AstType, trt: AstTraitType) -> AstTypeRestriction {
        AstTypeRestriction { ty, trt }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImplId(pub usize);

#[derive(Debug)]
pub struct AstImpl {
    pub impl_id: ImplId,
    pub name_span: Span,
    pub generics: Vec<String>,
    pub trait_ty: AstTraitType,
    pub impl_ty: AstType,
    pub fns: Vec<AstObjectFunction>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub associated_types: HashMap<String, AstType>,
}

lazy_static! {
    static ref IMPL_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}

impl AstImpl {
    pub fn new(
        name_span: Span,
        generics: Vec<String>,
        trait_ty: AstTraitType,
        impl_ty: AstType,
        fns: Vec<AstObjectFunction>,
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
}
