use crate::{
    lexer::Token,
    util::{FileId, FileRegistry, PError, Span},
};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    sync::RwLock,
};

pub mod display;
pub mod visitor;

pub enum AstError {
    DuplicatedMember(String),
}

#[cfg(feature = "parse")]
impl<L, T> From<AstError> for lalrpop_util::ParseError<L, T, PError> {
    fn from(e: AstError) -> Self {
        let AstError::DuplicatedMember(name) = e;

        lalrpop_util::ParseError::User {
            error: PError::new(format!("Duplicated name: `{}`", name)),
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Visit)]
pub struct AstProgram {
    pub modules: Vec<AstModule>,
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Visit, PartialOrd, Ord)]
pub enum ModuleRef {
    Denormalized(Vec<String>),
    Normalized(FileId, String),
}

// We don't need GC on these values.
#[cfg(feature = "lg")]
simple_empty_finalize_trace![
    VariableId,
    ModuleRef,
    AstMatchPattern,
    AstNamedVariable,
    AstExpression
];

impl ModuleRef {
    pub fn full_name(&self) -> String {
        match self {
            ModuleRef::Denormalized(path) => path.join("::"),
            ModuleRef::Normalized(file, name) =>
                format!("{}::{}", FileRegistry::mod_path(*file).join("::"), name),
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Visit)]
pub enum AstUse {
    Use(Vec<String>, String),
    UseAll(Vec<String>),
}

pub enum AstModuleMember {
    Use(bool, AstUse),
    Function(AstFunction),
    Object(AstObject),
    Trait(AstTrait),
    Enum(AstEnum),
    Impl(AstImpl),
    Global(AstGlobalVariable),
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Visit)]
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

    pub fn try_new_from_members(
        id: FileId,
        name: String,
        members: Vec<AstModuleMember>,
    ) -> Result<AstModule, AstError> {
        let mut names = HashSet::new();

        let mut uses = Vec::new();
        let mut pub_uses = Vec::new();

        let mut functions = HashMap::new();
        let mut objects = HashMap::new();
        let mut traits = HashMap::new();
        let mut enums = HashMap::new();
        let mut impls = HashMap::new();
        let mut globals = HashMap::new();

        for member in members {
            match member {
                AstModuleMember::Use(p, u) =>
                    if p {
                        pub_uses.push(u);
                    } else {
                        uses.push(u);
                    },
                AstModuleMember::Function(f) => {
                    if names.contains(&f.name) {
                        return Err(AstError::DuplicatedMember(f.name.clone()));
                    }

                    names.insert(f.name.clone());
                    functions.insert(f.name.clone(), f);
                },
                AstModuleMember::Object(o) => {
                    if names.contains(&o.name) {
                        return Err(AstError::DuplicatedMember(o.name.clone()));
                    }

                    names.insert(o.name.clone());
                    objects.insert(o.name.clone(), o);
                },
                AstModuleMember::Trait(t) => {
                    if names.contains(&t.name) {
                        return Err(AstError::DuplicatedMember(t.name.clone()));
                    }

                    names.insert(t.name.clone());
                    traits.insert(t.name.clone(), t);
                },
                AstModuleMember::Enum(e) => {
                    if names.contains(&e.name) {
                        return Err(AstError::DuplicatedMember(e.name.clone()));
                    }

                    names.insert(e.name.clone());
                    enums.insert(e.name.clone(), e);
                },
                AstModuleMember::Global(g) => {
                    if names.contains(&g.name) {
                        return Err(AstError::DuplicatedMember(g.name.clone()));
                    }

                    names.insert(g.name.clone());
                    globals.insert(g.name.clone(), g);
                },
                AstModuleMember::Impl(i) => {
                    impls.insert(i.impl_id, i);
                },
            }
        }

        Ok(AstModule::new(
            id, name, pub_uses, uses, functions, objects, traits, enums, impls, globals,
        ))
    }
}

lazy_static! {
    static ref GENERIC_ID_COUNTER: RwLock<usize> = RwLock::new(1);
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, VisitAnonymous)]
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

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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
    pub scope: Option<HashMap<VariableId, AstNamedVariable>>,
}

impl AstFunction {
    pub fn new_declaration(
        file: FileId,
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
    ) -> AstFunction {
        AstFunction {
            name_span,
            generics,
            module_ref: ModuleRef::Normalized(file, name.clone()),
            name,
            parameter_list,
            return_type,
            restrictions,
            definition: None,
            scope: None,
        }
    }

    pub fn new_definition(
        file: FileId,
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
        definition: AstBlock,
    ) -> AstFunction {
        AstFunction {
            name_span,
            generics,
            module_ref: ModuleRef::Normalized(file, name.clone()),
            name,
            parameter_list,
            return_type,
            restrictions,
            definition: Some(definition),
            scope: None,
        }
    }

    pub fn new_definition_from_expr(
        file: FileId,
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        parameter_list: Vec<AstNamedVariable>,
        return_type: AstType,
        restrictions: Vec<AstTypeRestriction>,
        definition: AstExpression,
    ) -> AstFunction {
        AstFunction {
            name_span,
            generics,
            module_ref: ModuleRef::Normalized(file, name.clone()),
            name,
            parameter_list,
            return_type,
            restrictions,
            definition: Some(AstBlock::new(vec![], definition)),
            scope: None,
        }
    }
}

lazy_static! {
    static ref VARIABLE_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, VisitAnonymous)]
pub struct VariableId(pub usize);

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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
            name,
            ty,
            id: VariableId(id_ref.clone()),
        }
    }

    pub fn quoted_new(span: Span, name: String, ty: AstType, id: VariableId) -> AstNamedVariable {
        AstNamedVariable { span, name, ty, id }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, VisitAnonymous)]
pub struct InferId(pub usize);

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, VisitAnonymous)]
pub struct GenericId(pub usize);

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, VisitAnonymous)]
pub struct DummyId(pub usize);

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Visit)]
pub struct AstTraitType {
    pub name: ModuleRef,
    pub generics: Vec<AstType>,
}

impl AstTraitType {
    pub fn new(name: ModuleRef, generics: Vec<AstType>) -> AstTraitType {
        AstTraitType { name, generics }
    }
}

pub enum AstTypeOrBinding {
    Type(AstType),
    Binding(String, AstType),
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Visit)]
pub struct AstTraitTypeWithAssocs {
    pub trt: AstTraitType,

    /// We store these bindings like this because HashMap<_, _> is not Hash
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

    pub fn fn_trait(arg_tys: Vec<AstType>, return_ty: AstType) -> AstTraitTypeWithAssocs {
        let call_trait =
            ModuleRef::Denormalized(vec!["std".into(), "operators".into(), "Call".into()]);

        let mut assoc_bindings = BTreeMap::new();
        assoc_bindings.insert("Return".to_owned(), return_ty);

        AstTraitTypeWithAssocs {
            trt: AstTraitType::new(call_trait, vec![AstType::tuple(arg_tys)]),
            assoc_bindings,
        }
    }

    pub fn try_flatten(
        name: ModuleRef,
        children: Vec<AstTypeOrBinding>,
    ) -> Result<AstTraitTypeWithAssocs, AstError> {
        let mut generics = vec![];
        let mut assoc_bindings = BTreeMap::new();

        for child in children {
            match child {
                AstTypeOrBinding::Type(t) => {
                    generics.push(t);
                },
                AstTypeOrBinding::Binding(n, t) => {
                    let existing = assoc_bindings.insert(n.clone(), t);

                    if existing.is_some() {
                        return Err(AstError::DuplicatedMember(n));
                    }
                },
            }
        }

        Ok(AstTraitTypeWithAssocs::new(name, generics, assoc_bindings))
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Visit, PartialOrd, Ord)]
/// A type as parsed by the Parser module.
pub enum AstType {
    Infer(InferId),

    Int,
    Float,
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

    // Only used in tr for representing `{}*` in globals. Eurgh.
    ClosureEnvType,

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

    DynamicType {
        trait_tys: BTreeSet<AstTraitTypeWithAssocs>,
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

    #[cfg(feature = "tr")]
    pub fn get_element(ty: &AstType) -> crate::util::PResult<AstType> {
        if let AstType::Array { ty } = ty {
            Ok(*ty.clone())
        } else {
            perror!("Trying to access element type of non-array: {:?}", ty)
        }
    }

    pub fn tuple(types: Vec<AstType>) -> AstType {
        AstType::Tuple { types }
    }

    pub fn none() -> AstType {
        AstType::Tuple { types: vec![] }
    }

    pub fn object_or_enum(object: ModuleRef, generics: Vec<AstType>) -> AstType {
        AstType::ObjectEnum(object, generics)
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

    pub fn dynamic_type(trait_tys: BTreeSet<AstTraitTypeWithAssocs>) -> AstType {
        AstType::DynamicType { trait_tys }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
/// A collection of statements, given by a `{}` block.
pub struct AstBlock {
    pub statements: Vec<AstStatement>,
    pub expression: Box<AstExpression>,
    pub scope: Option<HashMap<VariableId, AstNamedVariable>>,
}

impl AstBlock {
    pub fn new(statements: Vec<AstStatement>, expr: AstExpression) -> AstBlock {
        AstBlock {
            statements,
            expression: Box::new(expr),
            scope: None,
        }
    }

    pub fn empty(span: Span) -> AstBlock {
        AstBlock {
            statements: vec![],
            expression: Box::new(AstExpression::nothing(span)),
            scope: None,
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, VisitAnonymous)]
pub struct LoopId(pub usize);

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub enum AstStatement {
    Let {
        pattern: AstMatchPattern,
        value: AstExpression,
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

    pub fn expression_statement(expression: AstExpression) -> AstStatement {
        AstStatement::Expression { expression }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AstExpression {
    pub data: AstExpressionData,
    pub ty: AstType,
    pub span: Span,
}

type SubExpression = Box<AstExpression>;

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, VisitAnonymous)]
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
        params: Vec<AstMatchPattern>,
        return_ty: AstType,
        expr: SubExpression,

        captured: Option<Vec<(AstNamedVariable, AstNamedVariable)>>,
        scope: Option<HashMap<VariableId, AstNamedVariable>>,
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
    Break {
        label: Option<String>,
        id: Option<LoopId>,
        value: SubExpression,
    },
    Continue {
        label: Option<String>,
        id: Option<LoopId>,
    },
    Return {
        value: SubExpression,
    },
    Assert {
        condition: SubExpression,
    },
    ConditionalCompilation {
        branches: HashMap<String, AstBlock>,
    },
}

/// Convenience type for quasiquoting a label
pub struct AstLabel(pub String);

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, VisitAnonymous)]
pub enum InstructionArgument {
    Expression(AstExpression),
    Type(AstType),
    Anonymous(String),
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, VisitAnonymous)]
pub enum InstructionOutput {
    Type(AstType),
    Anonymous(String),
}

pub enum AstEnumConstructorArguments {
    Named(HashMap<String, AstExpression>),
    Positional(Vec<AstExpression>),
    Plain,
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

    pub fn quoted_while_loop(
        span: Span,
        label: Option<String>,
        id: LoopId,
        condition: AstExpression,
        block: AstBlock,
        else_block: AstBlock,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::While {
                id,
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

    pub fn enum_constructor_from_args(
        span: Span,
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        args: AstEnumConstructorArguments,
    ) -> AstExpression {
        match args {
            AstEnumConstructorArguments::Plain =>
                AstExpression::plain_enum_constructor(span, enumerable, generics, variant),
            AstEnumConstructorArguments::Positional(args) =>
                AstExpression::positional_enum_constructor(
                    span, enumerable, generics, variant, args,
                ),
            AstEnumConstructorArguments::Named(args) =>
                AstExpression::named_enum_constructor(span, enumerable, generics, variant, args),
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

    pub fn quoted_identifier(
        span: Span,
        identifier: String,
        variable_id: VariableId,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Identifier {
                name: identifier,
                variable_id: Some(variable_id),
            },
            ty: AstType::infer(),
        }
    }

    pub fn global_variable(span: Span, name: ModuleRef) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::GlobalVariable { name },
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

    pub fn array_literal(span: Span, elements: Vec<AstExpression>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ArrayLiteral { elements },
            ty: AstType::infer(),
        }
    }

    pub fn closure(
        span: Span,
        params: Vec<AstMatchPattern>,
        return_ty: AstType,
        expr: AstExpression,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Closure {
                params,
                return_ty,
                expr: Box::new(expr),
                captured: None,
                scope: None,
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
                fn_name,
                generics,
                args,
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

    pub fn quoted_static_call(
        span: Span,
        call_type: AstType,
        associated_trait: AstTraitTypeWithAssocs,
        fn_name: String,
        fn_generics: Vec<AstType>,
        args: Vec<AstExpression>,
        impl_id: ImplId,
        impl_generics: Vec<AstType>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait: Some(associated_trait),
                impl_signature: Some(AstImplSignature {
                    impl_id,
                    generics: impl_generics,
                }),
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
                idx,
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
        binop: BinOpKind,
        rhs: AstExpression,
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

    pub fn binop_from_token(
        span: Span,
        lhs: AstExpression,
        binop: Token,
        rhs: AstExpression,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::BinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                kind: BinOpKind::from_token(binop),
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
            data: AstExpressionData::Tuple { values: vec![] },
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

    pub fn return_statement(span: Span, value: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Return {
                value: Box::new(value),
            },
            ty: AstType::infer(),
        }
    }

    pub fn return_nothing(span: Span) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Return {
                value: Box::new(AstExpression::nothing(span)),
            },
            ty: AstType::infer(),
        }
    }

    pub fn break_stmt(span: Span, value: AstExpression, label: Option<String>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Break {
                value: Box::new(value),
                label,
                id: None,
            },
            ty: AstType::infer(),
        }
    }

    pub fn quoted_break_stmt(
        span: Span,
        value: AstExpression,
        label: Option<String>,
        id: Option<usize>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Break {
                value: Box::new(value),
                label,
                id: id.map(|i| LoopId(i)),
            },
            ty: AstType::infer(),
        }
    }

    pub fn continue_stmt(span: Span, label: Option<String>) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Continue { label, id: None },
            ty: AstType::infer(),
        }
    }

    pub fn quoted_continue_stmt(
        span: Span,
        label: Option<String>,
        id: Option<usize>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Continue {
                label,
                id: id.map(|i| LoopId(i)),
            },
            ty: AstType::infer(),
        }
    }

    pub fn assert_statement(span: Span, condition: AstExpression) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::Assert {
                condition: Box::new(condition),
            },
            ty: AstType::infer(),
        }
    }

    pub fn as_string(expr: AstExpression) -> AstExpression {
        AstExpression::transmute(expr.span, expr, AstType::String)
    }

    pub fn string_interpolation_begin(
        span: Span,
        begin: String,
        expr_string: AstExpression,
    ) -> AstExpression {
        AstExpression::binop(
            span,
            AstExpression::literal(span, AstLiteral::String(begin)),
            BinOpKind::Add,
            expr_string,
        )
    }

    pub fn string_interpolation_continue(
        span: Span,
        expr: AstExpression,
        cont: String,
        end_string: AstExpression,
    ) -> AstExpression {
        AstExpression::binop(
            span,
            AstExpression::as_string(expr),
            BinOpKind::Add,
            AstExpression::binop(
                span,
                AstExpression::literal(span, AstLiteral::String(cont)),
                BinOpKind::Add,
                end_string,
            ),
        )
    }

    pub fn string_interpolation_end(span: Span, expr: AstExpression, end: String) -> AstExpression {
        AstExpression::binop(
            span,
            AstExpression::as_string(expr),
            BinOpKind::Add,
            AstExpression::literal(span, AstLiteral::String(end)),
        )
    }

    pub fn conditional_compilation(
        span: Span,
        branches: HashMap<String, AstBlock>,
    ) -> AstExpression {
        AstExpression {
            span,
            data: AstExpressionData::ConditionalCompilation { branches },
            ty: AstType::infer(),
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AstMatchBranch {
    pub pattern: AstMatchPattern,
    pub expression: AstExpression,
    pub scope: Option<HashMap<VariableId, AstNamedVariable>>,
}

impl AstMatchBranch {
    pub fn new(pattern: AstMatchPattern, expression: AstExpression) -> AstMatchBranch {
        AstMatchBranch {
            pattern,
            expression,
            scope: None,
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AstMatchPattern {
    pub span: Span,
    pub data: AstMatchPatternData,
    pub ty: AstType,
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, VisitAnonymous)]
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
    pub fn underscore(span: Span, ty: AstType) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Underscore,
            ty,
            span,
        }
    }

    pub fn identifier(span: Span, name: String, ty: AstType) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Identifier(AstNamedVariable::new(span, name, ty.clone())),
            ty,
            span,
        }
    }

    pub fn quoted_named_variable(v: AstNamedVariable) -> AstMatchPattern {
        AstMatchPattern {
            ty: v.ty.clone(),
            span: v.span,
            data: AstMatchPatternData::Identifier(v),
        }
    }

    pub fn positional_enum(
        span: Span,
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: Vec<AstMatchPattern>,
        ignore_rest: bool,
        ty: AstType,
    ) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            },
            ty,
            span,
        }
    }

    pub fn named_enum(
        span: Span,
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        children: HashMap<String, AstMatchPattern>,
        ignore_rest: bool,
        ty: AstType,
    ) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            },
            ty,
            span,
        }
    }

    pub fn plain_enum(
        span: Span,
        enumerable: ModuleRef,
        generics: Vec<AstType>,
        variant: String,
        ty: AstType,
    ) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::PlainEnum {
                enumerable,
                generics,
                variant,
            },
            ty,
            span,
        }
    }

    pub fn empty(span: Span, ty: AstType) -> AstMatchPattern {
        AstMatchPattern {
            span,
            data: AstMatchPatternData::Tuple(vec![]),
            ty,
        }
    }

    pub fn tuple(span: Span, children: Vec<AstMatchPattern>, ty: AstType) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Tuple(children),
            ty,
            span,
        }
    }

    pub fn literal(span: Span, lit: AstLiteral, ty: AstType) -> AstMatchPattern {
        AstMatchPattern {
            data: AstMatchPatternData::Literal(lit),
            ty,
            span,
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub enum AstLiteral {
    True,
    False,
    String(String),
    Int(String),
    Float(String),
    Char(char),
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Clone, Copy, Debug, PartialEq, Eq, VisitAnonymous)]
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
    AndShort,
    OrShort,
}

impl BinOpKind {
    pub fn from_token(op: Token) -> BinOpKind {
        match op {
            Token::Star => BinOpKind::Multiply,
            Token::Slash => BinOpKind::Divide,
            Token::Modulo => BinOpKind::Modulo,
            Token::Plus => BinOpKind::Add,
            Token::Minus => BinOpKind::Subtract,
            Token::Lt => BinOpKind::Less,
            Token::Gt => BinOpKind::Greater,
            Token::GreaterEqual => BinOpKind::GreaterEqual,
            Token::LessEqual => BinOpKind::LessEqual,
            Token::NotEquals => BinOpKind::NotEqual,
            Token::EqualsEquals => BinOpKind::EqualsEquals,
            Token::And => BinOpKind::And,
            Token::Pipe => BinOpKind::Or,
            Token::AndShort => BinOpKind::AndShort,
            Token::PipeShort => BinOpKind::OrShort,
            _ => unreachable!("Unknown token {}", op),
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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
        name: String,
        generics: Vec<AstGeneric>,
        restrictions: Vec<AstTypeRestriction>,
        members: Vec<AstObjectMember>,
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

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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
    pub scope: Option<HashMap<VariableId, AstNamedVariable>>,
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
            scope: None,
        }
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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

pub enum AstTraitMember {
    Type(String, Vec<AstTraitTypeWithAssocs>),
    Function(AstObjectFunction),
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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

    pub fn try_new_from_members(
        file: FileId,
        name_span: Span,
        name: String,
        generics: Vec<AstGeneric>,
        restrictions: Vec<AstTypeRestriction>,
        members: Vec<AstTraitMember>,
    ) -> Result<AstTrait, AstError> {
        let mut functions = HashMap::new();
        let mut associated_types = HashMap::new();

        for member in members {
            match member {
                AstTraitMember::Type(name, restrictions) => {
                    if associated_types.contains_key(&name) {
                        return Err(AstError::DuplicatedMember(name));
                    }

                    associated_types.insert(name.clone(), AstAssociatedType { name, restrictions });
                },
                AstTraitMember::Function(f) => {
                    if functions.contains_key(&f.name) {
                        return Err(AstError::DuplicatedMember(f.name.clone()));
                    }

                    functions.insert(f.name.clone(), f);
                },
            }
        }

        Ok(AstTrait::new(
            file,
            name_span,
            name,
            generics,
            functions,
            restrictions,
            associated_types,
        ))
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AstAssociatedType {
    pub name: String,
    pub restrictions: Vec<AstTraitTypeWithAssocs>,
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Visit)]
pub struct AstTypeRestriction {
    pub ty: AstType,
    pub trt: AstTraitTypeWithAssocs,
}

impl AstTypeRestriction {
    pub fn new(ty: AstType, trt: AstTraitTypeWithAssocs) -> AstTypeRestriction {
        AstTypeRestriction { ty, trt }
    }

    pub fn flatten(
        restrictions: Vec<(AstType, Vec<AstTraitTypeWithAssocs>)>,
    ) -> Vec<AstTypeRestriction> {
        let mut ret = vec![];

        for (ty, mut trts) in restrictions {
            if trts.len() == 1 {
                ret.push(AstTypeRestriction::new(ty, trts.pop().unwrap()));
            } else {
                for trt in trts {
                    ret.push(AstTypeRestriction::new(ty.clone(), trt));
                }
            }
        }

        ret
    }
}

pub enum AstImplMember {
    Type(String, AstType),
    Function(AstObjectFunction),
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, VisitAnonymous)]
pub struct ImplId(pub usize);

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AstImpl {
    pub impl_id: ImplId,
    pub name_span: Span,

    pub generics: Vec<AstGeneric>,
    pub restrictions: Vec<AstTypeRestriction>,

    pub trait_ty: Option<AstTraitType>,
    pub impl_ty: AstType,

    pub fns: HashMap<String, AstObjectFunction>,
    pub associated_types: HashMap<String, AstType>,

    pub is_stub: bool,
}

lazy_static! {
    static ref IMPL_ID_COUNTER: RwLock<usize> = RwLock::new(1);
    static ref IMPL_ID_DUMMY: ImplId = ImplId(0);
}

impl AstImpl {
    pub fn new(
        name_span: Span,
        generics: Vec<AstGeneric>,
        trait_ty: Option<AstTraitType>,
        impl_ty: AstType,
        fns: HashMap<String, AstObjectFunction>,
        restrictions: Vec<AstTypeRestriction>,
        associated_types: HashMap<String, AstType>,
        is_stub: bool,
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
            is_stub,
        }
    }

    pub fn try_new_from_members(
        name_span: Span,
        generics: Vec<AstGeneric>,
        trait_ty: Option<AstTraitType>,
        impl_ty: AstType,
        restrictions: Vec<AstTypeRestriction>,
        members: Vec<AstImplMember>,
    ) -> Result<AstImpl, AstError> {
        let mut functions = HashMap::new();
        let mut associated_types = HashMap::new();

        for member in members {
            match member {
                AstImplMember::Type(name, ty) => {
                    if associated_types.contains_key(&name) {
                        return Err(AstError::DuplicatedMember(name));
                    }

                    associated_types.insert(name, ty);
                },
                AstImplMember::Function(f) => {
                    if functions.contains_key(&f.name) {
                        return Err(AstError::DuplicatedMember(f.name.clone()));
                    }

                    functions.insert(f.name.clone(), f);
                },
            }
        }

        Ok(AstImpl::new(
            name_span,
            generics,
            trait_ty,
            impl_ty,
            functions,
            restrictions,
            associated_types,
            false,
        ))
    }

    pub fn new_id() -> ImplId {
        let mut id_ref = IMPL_ID_COUNTER.write().unwrap();
        *id_ref += 1;

        ImplId(id_ref.clone())
    }
}

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
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

#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AstEnumVariant {
    pub name_span: Span,
    pub name: String,

    pub fields: Vec<AstType>,

    /// If Some, then this enum is structural and this maps field names to
    /// positions. If None, then this enum is positional, and we cannot use
    /// field names.
    pub field_names: Option<HashMap<String, usize>>,
}

impl AstEnumVariant {
    pub fn new_plain(name_span: Span, name: String) -> AstEnumVariant {
        AstEnumVariant {
            name_span,
            name,
            fields: vec![],
            field_names: None,
        }
    }

    pub fn new_named(
        name_span: Span,
        name: String,
        fields: Vec<(String, AstType)>,
    ) -> AstEnumVariant {
        let field_names = fields
            .iter()
            .enumerate()
            .map(|(i, (f, _))| (f.clone(), i))
            .collect();
        let fields = fields.into_iter().map(|(_, t)| t).collect();

        AstEnumVariant {
            name_span,
            name,
            fields,
            field_names: Some(field_names),
        }
    }

    pub fn new_positional(name_span: Span, name: String, fields: Vec<AstType>) -> AstEnumVariant {
        AstEnumVariant {
            name_span,
            name,
            fields,
            field_names: None,
        }
    }
}

/// Used in tyck
#[Adapter("crate::ast::visitor::AstAdapter")]
#[derive(Debug, Clone, PartialEq, Eq, Visit)]
pub struct AstImplSignature {
    pub impl_id: ImplId,
    pub generics: Vec<AstType>,
}
