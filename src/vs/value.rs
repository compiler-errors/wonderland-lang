use super::thread::VResult;
use crate::{inst::InstFunctionSignature, vs::heap::*};

#[derive(Clone, Debug)]
pub enum VorpalValue {
    Int(i64),
    Float(f64),
    String(String),
    /// Representative of tuples ("by-value" types)
    ValueCollection {
        // Tuples are immutable.
        values: Vec<VorpalValue>,
    },
    /// Representative of structs and arrays ("by-reference" types)
    HeapCollection {
        offset: usize,
        limit: usize,
        id: VorpalHeapPointer,
    },
    EnumVariant {
        variant: String,
        values: Vec<VorpalValue>,
    },
    GlobalFn(InstFunctionSignature),
    Closure(VorpalClosurePointer),
    DynamicBox(VorpalDynPointer),
    Awaitable(VorpalAwaitablePointer),
    Undefined,
}

impl VorpalValue {
    pub fn unit() -> VorpalValue {
        VorpalValue::ValueCollection { values: vec![] }
    }

    pub fn boolean(b: bool) -> VorpalValue {
        VorpalValue::Int(if b { 1 } else { 0 })
    }

    pub fn as_boolean(self) -> bool {
        if let VorpalValue::Int(i) = self {
            match i {
                0 => false,
                1 => true,
                i => unreachable!("ICE: Cannot make boolean out of VorpalValue::Int({})", i),
            }
        } else {
            unreachable!("ICE: Can only make boolean out of Int, got `{:?}`", self)
        }
    }

    pub fn get_tuple_idx(self, idx: usize) -> VorpalValue {
        if let VorpalValue::ValueCollection { values } = self {
            values
                .into_iter()
                .nth(idx)
                .expect("ICE: Tuple is too small to get indexed value.")
        } else {
            unreachable!("ICE: Can only get tuple child from Tuple, got `{:?}`", self)
        }
    }

    pub fn get_tuple_idx_mut(&mut self, idx: usize) -> &mut VorpalValue {
        if let VorpalValue::ValueCollection { values } = self {
            values
                .iter_mut()
                .nth(idx)
                .expect("ICE: Tuple is too small to get indexed value.")
        } else {
            unreachable!("ICE: Can only get tuple child from Tuple, got `{:?}`", self)
        }
    }

    pub fn get_object_slice(self, start: usize, end: usize) -> VResult<VorpalValue> {
        if let VorpalValue::HeapCollection { id, offset, limit } = &self {
            let adj_start = start + *offset;
            let adj_end = end + *offset;

            if adj_end < adj_start {
                return vorpal_panic!(
                    "ICE: Start of array slice ({}) cannot be greater than end ({}).",
                    start,
                    end
                );
            }

            if *limit < adj_end {
                return vorpal_panic!(
                    "ICE: End of array slice ({}) cannot be past end of array ({})",
                    end,
                    *limit - *offset
                );
            }

            Ok(VorpalValue::HeapCollection {
                id: *id,
                offset: adj_start,
                limit: adj_end,
            })
        } else {
            unreachable!("ICE: Can only take a slice of an array, got `{:?}`", self);
        }
    }
}
