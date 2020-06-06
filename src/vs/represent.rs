use crate::{
    ast::{
        AstBlock, AstExpression, AstMatchBranch, AstMatchPattern, AstStatement, AstType,
        InstructionArgument, InstructionOutput, LoopId, VariableId,
    },
    inst::{InstFunctionSignature, InstObjectFunctionSignature},
    util::{PError, Span},
    vs::value::VorpalValue,
};
use std::collections::HashMap;

pub type VResult<T> = Result<T, VError>;

pub enum VError {
    Exit(i64),
    Panic {
        stack_trace: Option<String>,
        error: PError,
    },
}

macro_rules! vorpal_panic_at {
    ($span:expr, $($arg:tt)*) => {
        Err(crate::vs::represent::VError::Panic { 
            stack_trace: None, 
            error: crate::util::PError::new_at($span, format!($($arg)*)) 
        })
    }
}

#[derive(Debug)]
pub enum VorpalControlState<'v> {
    Initial,
    Value(VorpalValue),
    Expression(&'v AstExpression),
    LvalExpression(&'v AstExpression),
    Statement(&'v AstStatement),
}

impl VorpalControlState<'_> {
    pub fn is_value(&self) -> bool {
        matches!(self, VorpalControlState::Value(_))
    }

    pub fn unwrap_value(self) -> VorpalValue {
        match self {
            VorpalControlState::Value(v) => v,
            c => unreachable!("ICE: Expected VorpalControlState::Value, got `{:?}`", c),
        }
    }
}

pub struct VorpalStack<'v> {
    pub control: Vec<VorpalControl<'v>>,
    pub variables: Vec<HashMap<VariableId, VorpalValue>>,
}

impl<'v> VorpalStack<'v> {
    pub fn new() -> VorpalStack<'v> {
        VorpalStack {
            control: vec![],
            variables: vec![],
        }
    }
}

pub enum VorpalThreadState<'v> {
    Complete(VorpalValue),
    Incomplete(VorpalStack<'v>),
}

#[derive(Debug)]
pub enum VorpalControl<'v> {
    ParkedState(VorpalControlState<'v>),

    Block {
        uneval_rev: Vec<&'v AstStatement>,
        expr: &'v AstExpression,
    },

    CallFn {
        fun: InstFunctionSignature, // TODO: I: Maybe assign functions IDs
        uneval_rev: Vec<&'v AstExpression>, /* TODO: I: Let's use a slice and chop it off the
                                     * front? */
        args: Vec<VorpalValue>, // TODO: I: Can we preallocate these arrays?
    },
    CallObjFn {
        fun: InstObjectFunctionSignature, // TODO: Maybe assign functions IDs
        uneval_rev: Vec<&'v AstExpression>,
        args: Vec<VorpalValue>,
    },

    CallBody, // TODO: Put some stack info here maybe? A name span?

    ApplyToLval(&'v AstExpression),
    ApplyToLet(&'v AstMatchPattern),

    // Given RHS (#0) and tuple indices (#1)
    ApplyRval {
        rval: VorpalValue,
        indices_rev: Vec<usize>,
    },

    ApplyRvalToObject {
        rval: VorpalValue,
        mem_idx: usize,
        indices_rev: Vec<usize>,
    },

    Tuple {
        uneval_rev: Vec<&'v AstExpression>,
        values: Vec<VorpalValue>,
    },
    Array {
        uneval_rev: Vec<&'v AstExpression>,
        values: Vec<VorpalValue>,
    },
    Object {
        uneval_rev: Vec<&'v AstExpression>,
        values: Vec<VorpalValue>,
    },
    Enum {
        variant: String,
        uneval_rev: Vec<&'v AstExpression>,
        values: Vec<VorpalValue>,
    },

    TupleAccess(Vec<usize>),
    ObjectAccess(usize),

    If {
        block: &'v AstBlock,
        else_block: &'v AstBlock,
    },
    Match {
        branches: &'v [AstMatchBranch],
    },
    PreWhile {
        id: LoopId,
        condition: &'v AstExpression,
        block: &'v AstBlock,
        else_block: &'v AstBlock,
        exit_value: Option<VorpalValue>,
    },
    PostWhile {
        id: LoopId,
        condition: &'v AstExpression,
        block: &'v AstBlock,
        else_block: &'v AstBlock,
        exit_value: Option<VorpalValue>,
    },

    Break(LoopId),
    Return,

    Instruction {
        instruction: &'v str,
        output: &'v InstructionOutput,
        uneval_rev: Vec<&'v InstructionArgument>,
        values: Vec<VorpalInstructionArgument<'v>>,
        span: Span,
    },
}

#[derive(Debug)]
pub enum VorpalInstructionArgument<'v> {
    Value(VorpalValue),
    Type(&'v AstType),
    Anonymous(&'v str),
}
