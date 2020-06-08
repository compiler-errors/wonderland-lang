use super::represent::{VResult, VorpalThread};
use crate::{
    ast::{AstExpression, AstMatchPattern, ModuleRef, VariableId},
    inst::{InstFunctionSignature, InstObjectFunctionSignature},
    util::Span,
};
use std::{
    collections::{HashMap, VecDeque},
    sync::atomic::{AtomicUsize, Ordering},
    time::Instant,
};

static VORPAL_POINTER_COUNTER: AtomicUsize = AtomicUsize::new(0);
static VORPAL_THREAD_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VorpalHeapPointer(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VorpalClosurePointer(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VorpalDynPointer(usize);

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

    pub fn get_object_len(&self) -> usize {
        if let VorpalValue::HeapCollection { offset, limit, .. } = self {
            limit - offset
        } else {
            unreachable!(
                "ICE: Can only get length from Array or Object, got `{:?}`",
                self
            )
        }
    }

    pub fn get_object_slice(self, start: usize, end: usize) -> VorpalValue {
        if let VorpalValue::HeapCollection { id, offset, limit } = &self {
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

            VorpalValue::HeapCollection {
                id: *id,
                offset: adj_start,
                limit: adj_end,
            }
        } else {
            unreachable!("ICE: Can only take a slice of an array, got `{:?}`", self);
        }
    }
}

#[derive(Clone, Debug)]
pub struct VorpalClosure<'v> {
    pub parameters: Vec<&'v AstMatchPattern>,
    pub captured: HashMap<VariableId, VorpalValue>,
    pub expression: &'v AstExpression,
}

#[derive(Clone, Debug)]
pub struct VorpalDynBox {
    pub type_id: usize,
    pub object: VorpalValue,
    pub table: HashMap<String, InstObjectFunctionSignature>,
}

pub struct VorpalHeap<'v> {
    pub yielded_threads: VecDeque<VorpalThread<'v>>,
    pub global_variables: HashMap<ModuleRef, VorpalValue>,

    pub heap_structs: HashMap<VorpalHeapPointer, Vec<VorpalValue>>,
    pub dyn_boxes: HashMap<VorpalDynPointer, VorpalDynBox>,
    pub closures: HashMap<VorpalClosurePointer, VorpalClosure<'v>>,
}

impl<'v> VorpalHeap<'v> {
    pub fn new() -> VorpalHeap<'v> {
        VorpalHeap {
            yielded_threads: VecDeque::new(),
            global_variables: HashMap::new(),
            heap_structs: HashMap::new(),
            dyn_boxes: HashMap::new(),
            closures: HashMap::new(),
        }
    }

    pub fn new_thread(&mut self, exit_type_id: usize) -> VorpalThread<'v> {
        let thread_id = VORPAL_THREAD_COUNTER.fetch_add(1, Ordering::Relaxed);

        VorpalThread {
            thread_id,
            exit_type_id,
            start: Instant::now(),
            control: vec![],
            variables: vec![],
            thread_object: self.allocate_heap_object(vec![
                /* id: Int */
                VorpalValue::Int(thread_id as i64),
                /* completed: Option<Dyn> */
                VorpalValue::EnumVariant {
                    variant: "None".to_string(),
                    values: vec![],
                },
            ]),
        }
    }

    pub fn get_global(&mut self, span: Span, r: &ModuleRef) -> VResult<&mut VorpalValue> {
        if let Some(v) = self.global_variables.get_mut(r) {
            Ok(v)
        } else {
            vorpal_panic_at!(
                span,
                "Global variable `{}` has not yet been initialized",
                r.full_name()
            )
        }
    }

    pub fn allocate_heap_object(&mut self, contents: Vec<VorpalValue>) -> VorpalValue {
        let limit = contents.len();
        let id = VorpalHeapPointer(VORPAL_POINTER_COUNTER.fetch_add(1, Ordering::Relaxed));

        self.heap_structs.insert(id, contents);

        VorpalValue::HeapCollection {
            id,
            offset: 0,
            limit,
        }
    }

    pub fn get_object_idx(&mut self, v: VorpalValue, idx: usize) -> &mut VorpalValue {
        if let VorpalValue::HeapCollection { id, offset, limit } = v {
            let adj_idx = idx + offset;
            assert!(adj_idx < limit);

            self.heap_structs
                .get_mut(&id)
                .expect("ICE: null pointer")
                .get_mut(idx)
                .expect("Object/Array is too small to get indexed value")
        } else {
            unreachable!(
                "ICE: Can only get heap child from Array or Object, got `{:?}`",
                v
            )
        }
    }

    pub fn allocate_dynamic_box(
        &mut self,
        type_id: usize,
        object: VorpalValue,
        table: HashMap<String, InstObjectFunctionSignature>,
    ) -> VorpalValue {
        let id = VorpalDynPointer(VORPAL_POINTER_COUNTER.fetch_add(1, Ordering::Relaxed));

        self.dyn_boxes.insert(id, VorpalDynBox {
            type_id,
            object,
            table,
        });

        VorpalValue::DynamicBox(id)
    }

    pub fn get_dyn_box(&self, c: VorpalDynPointer) -> &VorpalDynBox {
        &self.dyn_boxes[&c]
    }

    pub fn allocate_closure(
        &mut self,
        parameters: Vec<&'v AstMatchPattern>,
        captured: HashMap<VariableId, VorpalValue>,
        expression: &'v AstExpression,
    ) -> VorpalValue {
        let id = VorpalClosurePointer(VORPAL_POINTER_COUNTER.fetch_add(1, Ordering::Relaxed));

        self.closures.insert(id, VorpalClosure {
            parameters,
            captured,
            expression,
        });

        VorpalValue::Closure(id)
    }

    pub fn get_closure(&self, c: VorpalClosurePointer) -> &VorpalClosure<'v> {
        &self.closures[&c]
    }
}
