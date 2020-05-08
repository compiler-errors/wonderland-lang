use crate::{
    ast::{AstExpression, AstMatchPattern, AstNamedVariable, LoopId, ModuleRef},
    util::{PError, PResult},
};
use gc::{Gc, GcCell};
use gc_derive::*;

#[derive(Clone, Debug, Trace, Finalize)]
pub enum CheshireValue {
    Int(i64),
    Float(f64),
    String(String),
    /// Representative of tuples ("by-value" types)
    ValueCollection {
        // Tuples are immutable.
        contents: Vec<CheshireValue>,
    },
    /// Representative of structs and arrays ("by-reference" types)
    HeapCollection {
        contents: Gc<GcCell<Vec<CheshireValue>>>,
    },
    EnumVariant {
        variant: String,
        contents: Vec<CheshireValue>,
    },
    GlobalFn(ModuleRef),
    Closure {
        parameters: Vec<AstMatchPattern>,
        captured: Vec<(AstNamedVariable, AstNamedVariable)>,
        expression: AstExpression,
    },
    Undefined,
}

impl CheshireValue {
    pub fn int_from_bool(condition: bool) -> CheshireValue {
        if condition {
            CheshireValue::Int(1)
        } else {
            CheshireValue::Int(0)
        }
    }

    pub fn is_true(self) -> bool {
        match self {
            CheshireValue::Int(0) => false,
            CheshireValue::Int(1) => true,
            _ => unreachable!(),
        }
    }

    pub fn value_collection(contents: Vec<CheshireValue>) -> CheshireValue {
        CheshireValue::ValueCollection { contents }
    }

    pub fn heap_collection(contents: Vec<CheshireValue>) -> CheshireValue {
        CheshireValue::HeapCollection {
            contents: Gc::new(GcCell::new(contents)),
        }
    }

    pub fn enum_variant(variant: String, contents: Vec<CheshireValue>) -> CheshireValue {
        CheshireValue::EnumVariant { variant, contents }
    }

    pub fn closure(
        parameters: Vec<AstMatchPattern>,
        captured: Vec<(AstNamedVariable, AstNamedVariable)>,
        expression: AstExpression,
    ) -> CheshireValue {
        CheshireValue::Closure {
            parameters,
            captured,
            expression,
        }
    }

    pub fn get_member(&self, idx: usize) -> PResult<CheshireValue> {
        match self {
            CheshireValue::ValueCollection { contents } => Ok(contents[idx].clone()),
            CheshireValue::HeapCollection { contents } => Ok(contents.borrow()[idx].clone()),
            _ => perror!("Cannot access member (`{}`) of the type {:?}", idx, self),
        }
    }

    pub fn set_heap_member(&self, idx: usize, value: CheshireValue) -> PResult<()> {
        match self {
            CheshireValue::HeapCollection { contents } => {
                contents.borrow_mut()[idx] = value;
                Ok(())
            },
            _ => perror!("Cannot access member (`{}`) of the type {:?}", idx, self),
        }
    }

    pub fn get_tuple_member_mut(&mut self, idx: usize) -> PResult<&mut CheshireValue> {
        match self {
            CheshireValue::ValueCollection { contents } => Ok(&mut contents[idx]),
            _ => perror!("Cannot access member (`{}`) of the type {:?}", idx, self),
        }
    }

    pub fn unwrap_int(&self) -> PResult<i64> {
        match self {
            CheshireValue::Int(i) => Ok(*i),
            _ => perror!("Cannot unwrap value `{:?}` as Int", self),
        }
    }

    pub fn unwrap_float(&self) -> PResult<f64> {
        match self {
            CheshireValue::Float(f) => Ok(*f),
            _ => perror!("Cannot unwrap value `{:?}` as Float", self),
        }
    }

    pub fn unwrap_string(&self) -> PResult<String> {
        match self {
            CheshireValue::String(s) => Ok(s.clone()),
            _ => perror!("Cannot unwrap value `{:?}` as String", self),
        }
    }

    pub fn array_len(&self) -> PResult<usize> {
        match self {
            CheshireValue::HeapCollection { contents } => Ok(contents.borrow().len()),
            _ => perror!("Cannot unwrap value `{:?}` as String", self),
        }
    }

    pub fn string_len(&self) -> PResult<usize> {
        match self {
            CheshireValue::String(s) => Ok(s.len()),
            _ => perror!("Cannot unwrap value `{:?}` as String", self),
        }
    }
}

pub type LResult<T> = Result<T, LError>;

pub trait ShouldPopStack {
    fn should_pop_stack(&self) -> bool;
}

impl<T> ShouldPopStack for LResult<T> {
    fn should_pop_stack(&self) -> bool {
        matches!(self, Err(LError::Return(_)) | Err(LError::Continue(_)) | Err(LError::Break(..)))
    }
}

pub enum LError {
    InternalException(PError),
    Exit(i64),
    Return(CheshireValue),
    Break(LoopId, CheshireValue),
    Continue(LoopId),
}

impl From<PError> for LError {
    fn from(e: PError) -> LError {
        LError::InternalException(e)
    }
}
