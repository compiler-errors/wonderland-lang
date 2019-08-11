use crate::util::FileReader;

#[derive(Debug)]
/// A file that is being parsed, along with the associated
/// parsed functions that are contained in the file.
pub struct ParseFile<'a> {
    pub file: FileReader<'a>,
    pub functions: Vec<AstFunction>,
    pub export_fns: Vec<AstFnSignature>,
    pub objects: Vec<AstObject>,
    pub traits: Vec<AstTrait>,
    pub impls: Vec<AstImpl>,
}

impl<'a> ParseFile<'a> {
    pub fn new(
        file: FileReader<'a>,
        functions: Vec<AstFunction>,
        export_fns: Vec<AstFnSignature>,
        objects: Vec<AstObject>,
        traits: Vec<AstTrait>,
        impls: Vec<AstImpl>,
    ) -> ParseFile<'a> {
        ParseFile {
            file: file,
            functions: functions,
            export_fns: export_fns,
            objects: objects,
            traits: traits,
            impls: impls,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstFnSignature {
    /// The beginning position of the function
    /// The simple name of the function
    pub name: String,
    pub generics: Vec<String>,
    /// The parameter list that the function receives
    pub parameter_list: Vec<AstNamedVariable>,
    /// The return type of the function, or AstType::None
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstFnSignature {
    pub fn new(
        name: String,
        generics: Vec<String>,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstFnSignature {
        AstFnSignature {
            name: name,
            generics: generics,
            parameter_list: parameter_list,
            return_type: return_type,
            restrictions: restrictions,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
/// A parsed function
/// The variables associated with the function are given by
/// [beginning_of_vars, end_of_vars).
pub struct AstFunction {
    pub signature: AstFnSignature,
    /// The collection of statements associated with the function
    pub definition: AstBlock,
}

impl AstFunction {
    pub fn new(
        name: String,
        generics: Vec<String>,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
        definition: AstBlock,
    ) -> AstFunction {
        AstFunction {
            signature: AstFnSignature {
                generics: generics,
                name: name,
                parameter_list: parameter_list,
                return_type: return_type,
                restrictions: restrictions,
            },
            definition: definition,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A name and type associated with a parameter, along
/// with the position where this parameter is named.
pub struct AstNamedVariable {
    pub name: String,
    pub ty: AstType,

    /// Used in analyzer. Not populated before this.
    pub id: Option<usize>,
}

impl AstNamedVariable {
    pub fn new(name: String, ty: AstType) -> AstNamedVariable {
        AstNamedVariable {
            name: name,
            ty: ty,
            id: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A type as parsed by the Parser module.
pub enum AstType {
    Infer,
    Int,
    Char,
    Bool,
    String,
    SelfType,

    Generic(String),

    Array { ty: Box<AstType> },
    Tuple { types: Vec<AstType> },
    Object(String, Vec<AstType>),

    InferPlaceholder(usize),
    GenericPlaceholder(usize, String),
}

impl AstType {
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
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum AstStatement {
    Block {
        block: AstBlock,
    },
    Let {
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
    NoOp,
}

impl AstStatement {
    pub fn block(block: AstBlock) -> AstStatement {
        AstStatement::Block { block }
    }

    pub fn let_statement(var_name: String, ty: AstType, value: AstExpression) -> AstStatement {
        AstStatement::Let {
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
            value: AstExpression::Nothing,
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

    pub fn noop() -> AstStatement {
        AstStatement::NoOp
    }
}

type SubExpression = Box<AstExpression>;

#[derive(Debug, Eq, PartialEq)]
pub enum AstExpression {
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
        object: Box<AstExpression>,
        fn_name: String,
        generics: Vec<AstType>,
        args: Vec<AstExpression>,
    },
    /// Call an object's static function
    StaticCall {
        obj_name: String,
        obj_generics: Vec<AstType>,
        fn_name: String,
        fn_generics: Vec<AstType>,
        args: Vec<AstExpression>,
    },
    /// An array access `a[1u]`
    Access {
        accessible: Box<AstExpression>,
        idx: Box<AstExpression>,
    },
    /// A tuple access `a:1`
    TupleAccess {
        accessible: Box<AstExpression>,
        idx: usize,
    },
    /// Call an object's member
    ObjectAccess {
        object: Box<AstExpression>,
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

    /// For use after analysis step.
    VariableIdx(usize),
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
    Set,
}

impl AstExpression {
    pub fn string_literal(string: String, len: usize) -> AstExpression {
        AstExpression::String { string, len }
    }

    pub fn char_literal(ch: char) -> AstExpression {
        AstExpression::Char(ch)
    }

    pub fn int_literal(num: String) -> AstExpression {
        AstExpression::Int(num)
    }

    pub fn identifier(identifier: String) -> AstExpression {
        AstExpression::Identifier { name: identifier }
    }

    pub fn tuple_literal(values: Vec<AstExpression>) -> AstExpression {
        AstExpression::Tuple { values }
    }

    pub fn empty_array_literal() -> AstExpression {
        AstExpression::Array {
            elements: Vec::new(),
        }
    }

    pub fn array_literal(elements: Vec<AstExpression>) -> AstExpression {
        AstExpression::Array { elements }
    }

    pub fn call(name: String, generics: Vec<AstType>, args: Vec<AstExpression>) -> AstExpression {
        AstExpression::Call {
            name,
            generics,
            args,
        }
    }

    pub fn object_call(
        object: AstExpression,
        fn_name: String,
        generics: Vec<AstType>,
        args: Vec<AstExpression>,
    ) -> AstExpression {
        AstExpression::ObjectCall {
            object: Box::new(object),
            fn_name: fn_name,
            generics: generics,
            args: args,
        }
    }

    pub fn static_call(
        obj_name: String,
        obj_generics: Vec<AstType>,
        fn_name: String,
        fn_generics: Vec<AstType>,
        args: Vec<AstExpression>,
    ) -> AstExpression {
        AstExpression::StaticCall {
            obj_name: obj_name,
            obj_generics: obj_generics,
            fn_name: fn_name,
            fn_generics: fn_generics,
            args: args,
        }
    }

    pub fn access(lhs: AstExpression, idx: AstExpression) -> AstExpression {
        AstExpression::Access {
            accessible: Box::new(lhs),
            idx: Box::new(idx),
        }
    }

    pub fn tuple_access(lhs: AstExpression, idx: usize) -> AstExpression {
        AstExpression::TupleAccess {
            accessible: Box::new(lhs),
            idx: idx,
        }
    }

    pub fn object_access(object: AstExpression, mem_name: String) -> AstExpression {
        AstExpression::ObjectAccess {
            object: Box::new(object),
            mem_name,
        }
    }

    pub fn allocate(object: AstType) -> AstExpression {
        AstExpression::Allocate { object }
    }

    pub fn binop(lhs: AstExpression, rhs: AstExpression, binop: BinOpKind) -> AstExpression {
        AstExpression::BinOp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            kind: binop,
        }
    }

    pub fn not(lhs: AstExpression) -> AstExpression {
        AstExpression::Not(Box::new(lhs))
    }

    pub fn neg(lhs: AstExpression) -> AstExpression {
        AstExpression::Negate(Box::new(lhs))
    }

    pub fn nothing() -> AstExpression {
        AstExpression::Nothing
    }

    pub fn true_lit() -> AstExpression {
        AstExpression::True
    }

    pub fn false_lit() -> AstExpression {
        AstExpression::False
    }

    pub fn null_lit() -> AstExpression {
        AstExpression::Null
    }

    pub fn self_ref() -> AstExpression {
        AstExpression::SelfRef
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstObject {
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
        generics: Vec<String>,
        name: String,
        members: Vec<AstObjectMember>,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstObject {
        AstObject {
            generics: generics,
            name: name,
            restrictions: restrictions,
            members: members,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstObjectFnSignature {
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
}

impl AstObjectFnSignature {
    pub fn new(
        name: String,
        generics: Vec<String>,
        has_self: bool,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstObjectFnSignature {
        AstObjectFnSignature {
            name: name,
            generics: generics,
            has_self: has_self,
            parameter_list: parameter_list,
            return_type: return_type,
            restrictions: restrictions,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstObjectFunction {
    pub signature: AstObjectFnSignature,
    /// The collection of statements associated with the function
    pub definition: AstBlock,
}

impl AstObjectFunction {
    pub fn new(sig: AstObjectFnSignature, definition: AstBlock) -> AstObjectFunction {
        AstObjectFunction {
            signature: sig,
            definition: definition,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstObjectMember {
    pub name: String,
    pub member_type: AstType,
}

impl AstObjectMember {
    pub fn new(name: String, member_type: AstType) -> AstObjectMember {
        AstObjectMember {
            name: name,
            member_type: member_type,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstTrait {
    pub name: String,
    pub generics: Vec<String>,
    pub functions: Vec<AstObjectFnSignature>,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstTrait {
    pub fn new(
        name: String,
        generics: Vec<String>,
        functions: Vec<AstObjectFnSignature>,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstTrait {
        AstTrait {
            name: name,
            generics: generics,
            functions: functions,
            restrictions: restrictions,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstTypeRestriction {
    pub ty: AstType,
    pub trt: AstType,
}

impl AstTypeRestriction {
    pub fn new(ty: AstType, trt: AstType) -> AstTypeRestriction {
        AstTypeRestriction { ty, trt }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstImpl {
    pub generics: Vec<String>,
    pub trait_ty: AstType,
    pub impl_ty: AstType,
    pub fns: Vec<AstObjectFunction>,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstImpl {
    pub fn new(
        generics: Vec<String>,
        trait_ty: AstType,
        impl_ty: AstType,
        fns: Vec<AstObjectFunction>,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstImpl {
        AstImpl {
            generics: generics,
            trait_ty: trait_ty,
            impl_ty: impl_ty,
            restrictions: restrictions,
            fns: fns,
        }
    }
}
