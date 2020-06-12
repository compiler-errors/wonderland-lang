use super::heap::VorpalAwaitablePointer;
use crate::{
    ast::{
        AstBlock, AstExpression, AstMatchBranch, AstMatchPattern, AstStatement, AstType,
        InstructionArgument, InstructionOutput, LoopId, VariableId,
    },
    inst::{InstFunctionSignature, InstObjectFunctionSignature},
    util::{PError, Span},
    vs::value::VorpalValue,
};
use std::{
    collections::{HashMap, HashSet},
    time::Instant,
};

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
        Err(crate::vs::thread::VError::Panic {
            stack_trace: None,
            error: crate::util::PError::new_at($span, format!($($arg)*))
        })
    }
}

macro_rules! vorpal_panic {
    ($($arg:tt)*) => {
        Err(crate::vs::thread::VError::Panic {
            stack_trace: None,
            error: crate::util::PError::new(format!($($arg)*))
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ThreadId(pub usize);

impl std::fmt::Display for ThreadId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct VorpalThread<'v> {
    pub thread_id: ThreadId,

    pub control: Vec<VorpalControl<'v>>,
    pub variables: Vec<HashMap<VariableId, VorpalValue>>,

    pub blocking: HashSet<ThreadId>,
    pub blocked_on: HashSet<ThreadId>,

    pub exit_type_id: usize,
    pub start: Instant,
    pub thread_object: VorpalValue,
}

pub enum VorpalThreadState {
    Complete(VorpalValue),
    Incomplete,
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
        span: Span,
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
    AwaitPrePoll {
        span: Span,
        fun: InstObjectFunctionSignature,
    },
    AwaitPostPoll {
        span: Span,
        fun: InstObjectFunctionSignature,
    },

    Instruction {
        instruction: &'v str,
        output: &'v InstructionOutput,
        uneval_rev: Vec<&'v InstructionArgument>,
        values: Vec<VorpalInstructionArgument<'v>>,
        span: Span,
    },

    Async(VorpalAwaitablePointer),
}

#[derive(Debug)]
pub enum VorpalInstructionArgument<'v> {
    Value(VorpalValue),
    Type(&'v AstType),
    Anonymous(&'v str),
}
