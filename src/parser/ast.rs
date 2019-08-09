use util::FileReader;
use analyzer::{Ty, TY_NOTHING, VarId, StringId, ObjId, TraitId, MemberId};

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
    pub fn new(file: FileReader<'a>,
               functions: Vec<AstFunction>,
               export_fns: Vec<AstFnSignature>,
               objects: Vec<AstObject>,
               traits: Vec<AstTrait>,
               impls: Vec<AstImpl>)
               -> ParseFile<'a> {
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

#[derive(Debug, PartialEq, Eq)]
pub struct AstFnSignature {
    /// The beginning position of the function
    pub pos: usize,
    /// The simple name of the function
    pub name: String,
    pub generics: Vec<String>,
    /// The parameter list that the function receives
    pub parameter_list: Vec<AstFnParameter>,
    /// The return type of the function, or AstType::None
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstFnSignature {
    pub fn new(pos: usize,
               name: String,
               generics: Vec<String>,
               parameter_list: Vec<AstFnParameter>,
               return_type: AstType,
               restrictions: Vec<AstTypeRestriction>)
               -> AstFnSignature {
        AstFnSignature {
            pos: pos,
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
    /// The first variable ID associated with the function
    pub beginning_of_vars: VarId,
    /// The last variable ID associated with the function
    pub end_of_vars: VarId,
}

impl AstFunction {
    pub fn new(pos: usize,
               name: String,
               generics: Vec<String>,
               parameter_list: Vec<AstFnParameter>,
               return_type: AstType,
               restrictions: Vec<AstTypeRestriction>,
               definition: AstBlock)
               -> AstFunction {
        AstFunction {
            signature: AstFnSignature {
                pos: pos,
                generics: generics,
                name: name,
                parameter_list: parameter_list,
                return_type: return_type,
                restrictions: restrictions,
            },
            definition: definition,
            beginning_of_vars: VarId(0),
            end_of_vars: VarId(0),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
/// A name and type associated with a parameter, along
/// with the position where this parameter is named.
pub struct AstFnParameter {
    pub name: String,
    pub ty: AstType,
    pub pos: usize,
}

impl AstFnParameter {
    pub fn new(name: String, ty: AstType, pos: usize) -> AstFnParameter {
        AstFnParameter {
            name: name,
            ty: ty,
            pos: pos,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
/// A type as parsed by the Parser module.
pub enum AstType {
    Infer,
    None,
    Int,
    UInt,
    Float,
    Char,
    Bool,
    String,
    Array { ty: Box<AstType> },
    Tuple { types: Vec<AstType> },
    Object(String, Vec<AstType>, usize), // TODO: pos is tacked on hackily...
}

impl AstType {
    pub fn array(ty: AstType) -> AstType {
        AstType::Array { ty: Box::new(ty) }
    }

    pub fn tuple(types: Vec<AstType>) -> AstType {
        AstType::Tuple { types: types }
    }

    pub fn object(obj: String, generics: Vec<AstType>, pos: usize) -> AstType {
        AstType::Object(obj, generics, pos)
    }
}

#[derive(Debug, PartialEq, Eq)]
/// A collection of statements, given by a `{}` block.
pub struct AstBlock {
    pub statements: Vec<AstStatement>,
}

impl AstBlock {
    pub fn new(statements: Vec<AstStatement>) -> AstBlock {
        AstBlock { statements: statements }
    }

    pub fn empty() -> AstBlock {
        AstBlock { statements: Vec::new() }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstStatement {
    /// The actual Statement enum
    pub stmt: AstStatementData,
    /// The position of the statement
    pub pos: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstStatementData {
    Block { block: AstBlock },
    Let {
        var_name: String,
        ty: AstType,
        value: AstExpression,
        var_id: VarId,
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
    Return { value: AstExpression },
    Assert { condition: AstExpression },
    Expression { expression: AstExpression },
    NoOp,
}

impl AstStatement {
    pub fn block(block: AstBlock, pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::Block { block: block },
            pos: pos,
        }
    }

    pub fn let_statement(var_name: String,
                         ty: AstType,
                         value: AstExpression,
                         pos: usize)
                         -> AstStatement {
        AstStatement {
            stmt: AstStatementData::Let {
                var_name: var_name,
                ty: ty,
                value: value,
                var_id: VarId(0),
            },
            pos: pos,
        }
    }

    pub fn if_statement(condition: AstExpression,
                        block: AstBlock,
                        else_block: AstBlock,
                        pos: usize)
                        -> AstStatement {
        AstStatement {
            stmt: AstStatementData::If {
                condition: condition,
                block: block,
                else_block: else_block,
            },
            pos: pos,
        }
    }

    pub fn while_loop(condition: AstExpression, block: AstBlock, pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::While {
                condition: condition,
                block: block,
            },
            pos: pos,
        }
    }

    pub fn return_statement(value: AstExpression, pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::Return { value: value },
            pos: pos,
        }
    }

    pub fn return_nothing(pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::Return {
                value: AstExpression {
                    expr: AstExpressionData::Nothing,
                    ty: TY_NOTHING,
                    pos: pos,
                },
            },
            pos: pos,
        }
    }


    pub fn assert_statement(condition: AstExpression, pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::Assert { condition: condition },
            pos: pos,
        }
    }


    pub fn expression_statement(expression: AstExpression) -> AstStatement {
        let pos = expression.pos;
        AstStatement {
            stmt: AstStatementData::Expression { expression: expression },
            pos: pos,
        }
    }

    pub fn break_stmt(pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::Break,
            pos: pos,
        }
    }

    pub fn continue_stmt(pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::Continue,
            pos: pos,
        }
    }

    pub fn noop(pos: usize) -> AstStatement {
        AstStatement {
            stmt: AstStatementData::NoOp,
            pos: pos,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct AstExpression {
    pub expr: AstExpressionData,
    pub ty: Ty,
    pub pos: usize,
}

type SubExpression = Box<AstExpression>;

#[derive(Debug, Eq, PartialEq)]
pub enum AstExpressionData {
    Nothing,
    True,
    False,
    Null,
    SelfRef,
    String {
        string: String,
        len: u32,
        id: StringId,
    },
    Int(String),
    UInt(String),
    Float(String),
    Char(char),
    Identifier { name: String, var_id: VarId },
    Tuple { values: Vec<AstExpression> },
    Array { elements: Vec<AstExpression> },

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
        idx: u32,
    },
    /// Call an object's member
    ObjectAccess {
        object: Box<AstExpression>,
        mem_name: String,
        mem_idx: MemberId,
    },

    Allocate { object: AstType },

    Not(SubExpression),
    Negate(SubExpression),

    BinOp {
        kind: BinOpKind,
        lhs: SubExpression,
        rhs: SubExpression,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// The kind of binary operation
pub enum BinOpKind {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    ShiftLeft,
    ShiftRight,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    EqualsEquals,
    NotEqual,
    Xor,
    And,
    Or,
    Set,
}

impl AstExpression {
    pub fn string_literal(string: String, len: u32, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::String {
                string: string,
                len: len,
                id: StringId(0),
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn char_literal(ch: char, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Char(ch),
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn int_literal(num: String, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Int(num),
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn uint_literal(num: String, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::UInt(num),
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn float_literal(num: String, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Float(num),
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn identifier(identifier: String, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Identifier {
                name: identifier,
                var_id: VarId(0),
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn tuple_literal(values: Vec<AstExpression>, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Tuple { values: values },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn empty_array_literal(pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Array { elements: Vec::new() },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn array_literal(elements: Vec<AstExpression>, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Array { elements: elements },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn call(name: String,
                generics: Vec<AstType>,
                args: Vec<AstExpression>,
                pos: usize)
                -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Call {
                name: name,
                generics: generics,
                args: args,
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn object_call(object: AstExpression,
                       fn_name: String,
                       generics: Vec<AstType>,
                       args: Vec<AstExpression>,
                       pos: usize)
                       -> AstExpression {
        AstExpression {
            expr: AstExpressionData::ObjectCall {
                object: Box::new(object),
                fn_name: fn_name,
                generics: generics,
                args: args,
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn static_call(obj_name: String,
                       obj_generics: Vec<AstType>,
                       fn_name: String,
                       fn_generics: Vec<AstType>,
                       args: Vec<AstExpression>,
                       pos: usize)
                       -> AstExpression {
        AstExpression {
            expr: AstExpressionData::StaticCall {
                obj_name: obj_name,
                obj_generics: obj_generics,
                fn_name: fn_name,
                fn_generics: fn_generics,
                args: args,
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn access(lhs: AstExpression, idx: AstExpression, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Access {
                accessible: Box::new(lhs),
                idx: Box::new(idx),
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn tuple_access(lhs: AstExpression, idx: u32, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::TupleAccess {
                accessible: Box::new(lhs),
                idx: idx,
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn object_access(object: AstExpression, mem_name: String, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::ObjectAccess {
                object: Box::new(object),
                mem_name: mem_name,
                mem_idx: MemberId(0),
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn allocate(object: AstType, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Allocate { object: object },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn binop(lhs: AstExpression,
                 rhs: AstExpression,
                 binop: BinOpKind,
                 pos: usize)
                 -> AstExpression {
        AstExpression {
            expr: AstExpressionData::BinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind: binop,
            },
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn not(lhs: AstExpression, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Not(Box::new(lhs)),
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn neg(lhs: AstExpression, pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Negate(Box::new(lhs)),
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn nothing(pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Nothing,
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn true_lit(pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::True,
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn false_lit(pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::False,
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn null_lit(pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::Null,
            ty: Ty(0),
            pos: pos,
        }
    }

    pub fn self_ref(pos: usize) -> AstExpression {
        AstExpression {
            expr: AstExpressionData::SelfRef,
            ty: Ty(0),
            pos: pos,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstObject {
    /// The beginning position of the object
    pub pos: usize,
    pub generics: Vec<String>,
    /// The object name
    pub name: String,
    /// The Id associated with the object in Analysis
    pub id: ObjId,
    /// The members that are contained in the object
    pub members: Vec<AstObjectMember>,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstObject {
    pub fn new(pos: usize,
               generics: Vec<String>,
               name: String,
               members: Vec<AstObjectMember>,
               restrictions: Vec<AstTypeRestriction>)
               -> AstObject {
        AstObject {
            pos: pos,
            generics: generics,
            name: name,
            id: ObjId(0),
            restrictions: restrictions,
            members: members,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstObjectFnSignature {
    /// The beginning position of the function
    pub pos: usize,
    /// The simple name of the function
    pub name: String,
    pub generics: Vec<String>,
    /// Whether the function is a member or static function of the type
    pub has_self: bool,
    /// The parameter list that the function receives
    pub parameter_list: Vec<AstFnParameter>,
    /// The return type of the function, or AstType::None
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstObjectFnSignature {
    pub fn new(pos: usize,
               name: String,
               generics: Vec<String>,
               has_self: bool,
               parameter_list: Vec<AstFnParameter>,
               return_type: AstType,
               restrictions: Vec<AstTypeRestriction>)
               -> AstObjectFnSignature {
        AstObjectFnSignature {
            pos: pos,
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
    /// The first variable ID associated with the function
    pub beginning_of_vars: VarId,
    /// The last variable ID associated with the function
    pub end_of_vars: VarId,
}

impl AstObjectFunction {
    pub fn new(sig: AstObjectFnSignature, definition: AstBlock) -> AstObjectFunction {
        AstObjectFunction {
            signature: sig,
            definition: definition,
            beginning_of_vars: VarId(0),
            end_of_vars: VarId(0),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstObjectMember {
    pub name: String,
    pub member_type: AstType,
    pub ty: Ty,
    pub pos: usize,
}

impl AstObjectMember {
    pub fn new(name: String, member_type: AstType, pos: usize) -> AstObjectMember {
        AstObjectMember {
            name: name,
            member_type: member_type,
            ty: Ty(0),
            pos: pos,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstTrait {
    pub pos: usize,
    pub name: String,
    pub generics: Vec<String>,
    pub functions: Vec<AstObjectFnSignature>,
    pub id: TraitId,
    pub restrictions: Vec<AstTypeRestriction>,
}

impl AstTrait {
    pub fn new(pos: usize,
               name: String,
               generics: Vec<String>,
               functions: Vec<AstObjectFnSignature>,
               restrictions: Vec<AstTypeRestriction>)
               -> AstTrait {
        AstTrait {
            name: name,
            generics: generics,
            functions: functions,
            id: TraitId(0),
            restrictions: restrictions,
            pos: pos,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstTypeRestriction {
    pub pos: usize,
    pub ty: AstType,
    pub trt: AstType,
}

impl AstTypeRestriction {
    pub fn new(pos: usize, ty: AstType, trt: AstType) -> AstTypeRestriction {
        AstTypeRestriction {
            pos: pos,
            ty: ty,
            trt: trt,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstImpl {
    pub generics: Vec<String>,
    pub trait_ty: AstType,
    pub impl_ty: AstType,
    pub fns: Vec<AstObjectFunction>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub pos: usize,
}

impl AstImpl {
    pub fn new(pos: usize,
               generics: Vec<String>,
               trait_ty: AstType,
               impl_ty: AstType,
               fns: Vec<AstObjectFunction>,
               restrictions: Vec<AstTypeRestriction>)
               -> AstImpl {
        AstImpl {
            pos: pos,
            generics: generics,
            trait_ty: trait_ty,
            impl_ty: impl_ty,
            restrictions: restrictions,
            fns: fns,
        }
    }
}
