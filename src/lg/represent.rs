use crate::{
    ast::{AstExpression, AstMatchPattern, LoopId, ModuleRef, VariableId},
    inst::InstObjectFunctionSignature,
    util::{PError, PResult},
};
use gc::{Gc, GcCell};
use gc_derive::*;
use std::collections::HashMap;

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
        offset: usize,
        limit: usize,
        contents: Gc<GcCell<Vec<CheshireValue>>>,
    },
    EnumVariant {
        variant: String,
        contents: Vec<CheshireValue>,
    },
    GlobalFn(ModuleRef),
    Closure(Gc<LgClosure>),
    DynamicBox(Gc<LgDynamicBox>),
    Undefined,
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct LgClosure {
    pub parameters: Vec<AstMatchPattern>,
    pub captured: HashMap<VariableId, CheshireValue>,
    pub expression: AstExpression,
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct LgDynamicBox {
    pub type_id: usize,
    pub object: CheshireValue,
    pub table: HashMap<String, InstObjectFunctionSignature>,
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
            offset: 0,
            limit: contents.len(),
            contents: Gc::new(GcCell::new(contents)),
        }
    }

    pub fn enum_variant(variant: String, contents: Vec<CheshireValue>) -> CheshireValue {
        CheshireValue::EnumVariant { variant, contents }
    }

    pub fn closure(
        parameters: Vec<AstMatchPattern>,
        captured: HashMap<VariableId, CheshireValue>,
        expression: AstExpression,
    ) -> CheshireValue {
        CheshireValue::Closure(Gc::new(LgClosure {
            parameters,
            captured,
            expression,
        }))
    }

    pub fn dynamic_box(
        type_id: usize,
        object: CheshireValue,
        table: HashMap<String, InstObjectFunctionSignature>,
    ) -> CheshireValue {
        CheshireValue::DynamicBox(Gc::new(LgDynamicBox {
            type_id,
            object,
            table,
        }))
    }

    pub fn get_array_slice(self, start: usize, end: usize) -> CheshireValue {
        if let CheshireValue::HeapCollection {
            contents,
            offset,
            limit,
        } = &self
        {
            let adj_start = start + *offset;
            let adj_end = end + *offset;

            if adj_end < adj_start {
                unreachable!(
                    "ICE: Start of array slice ({}) cannot be greater than end ({}).",
                    adj_start, adj_end
                );
            }

            if *limit < adj_end {
                unreachable!(
                    "ICE: End of array slice ({}) cannot be past end of array ({})",
                    adj_end, limit
                );
            }

            CheshireValue::HeapCollection {
                contents: contents.clone(),
                offset: adj_start,
                limit: adj_end,
            }
        } else {
            unreachable!("ICE: Can only take a slice of an array, got `{:?}`", self);
        }
    }

    pub fn get_member(&self, idx: usize) -> PResult<CheshireValue> {
        match self {
            CheshireValue::ValueCollection { contents } => Ok(contents[idx].clone()),
            CheshireValue::HeapCollection {
                contents,
                offset,
                limit,
            } => {
                let adj_idx = *offset + idx;

                if adj_idx >= *limit {
                    unreachable!("ICE: Index ({}) is out of bounds ({}).", adj_idx, limit);
                }

                Ok(contents.borrow()[adj_idx].clone())
            },
            _ => perror!("Cannot access member (`{}`) of the type {:?}", idx, self),
        }
    }

    pub fn set_heap_member(&self, idx: usize, value: CheshireValue) -> PResult<()> {
        match self {
            CheshireValue::HeapCollection {
                contents,
                offset,
                limit,
            } => {
                let adj_idx = *offset + idx;

                if adj_idx >= *limit {
                    unreachable!("ICE: Index ({}) is out of bounds ({}).", adj_idx, limit);
                }

                // I don't want to return a member_mut because I unwrap the GC cell here,
                // so I'd have to use Rental or something to return both the Gc (clone),
                // but also the mutable member...
                contents.borrow_mut()[adj_idx] = value;
                Ok(())
            },
            _ => perror!(
                "Cannot access object member (`{}`) of the type {:?}",
                idx,
                self
            ),
        }
    }

    pub fn get_tuple_member_mut(&mut self, idx: usize) -> PResult<&mut CheshireValue> {
        match self {
            CheshireValue::ValueCollection { contents } => Ok(&mut contents[idx]),
            _ => perror!(
                "ICE: Cannot access tuple member (`{}`) of the type {:?}",
                idx,
                self
            ),
        }
    }

    pub fn unwrap_int(&self) -> PResult<i64> {
        match self {
            CheshireValue::Int(i) => Ok(*i),
            _ => unreachable!("ICE: Cannot unwrap value `{:?}` as Int", self),
        }
    }

    pub fn unwrap_float(&self) -> PResult<f64> {
        match self {
            CheshireValue::Float(f) => Ok(*f),
            _ => unreachable!("ICE: Cannot unwrap value `{:?}` as Float", self),
        }
    }

    pub fn unwrap_string(&self) -> PResult<String> {
        match self {
            CheshireValue::String(s) => Ok(s.clone()),
            _ => unreachable!("ICE: Cannot unwrap value `{:?}` as String", self),
        }
    }

    pub fn array_len(&self) -> PResult<usize> {
        match self {
            CheshireValue::HeapCollection { offset, limit, .. } => Ok(*limit - *offset),
            _ => unreachable!("ICE: Cannot unwrap array length of `{:?}`", self),
        }
    }

    pub fn string_len(&self) -> PResult<usize> {
        match self {
            CheshireValue::String(s) => Ok(s.len()),
            _ => unreachable!("ICE: Cannot unwrap string length of `{:?}`", self),
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
