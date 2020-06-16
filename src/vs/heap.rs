use super::{
    exported::VorpalExternFn,
    thread::{ThreadId, VResult, VorpalControl, VorpalControlState, VorpalThread},
    value::VorpalValue,
};
use crate::{
    ast::{AstExpression, AstMatchPattern, ModuleRef, VariableId},
    inst::InstObjectFunctionSignature,
    util::Span,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VorpalAwaitablePointer(usize);

#[derive(Clone, Debug)]
pub struct VorpalClosure<'v> {
    pub parameters: Vec<&'v AstMatchPattern>,
    pub captured: HashMap<VariableId, VorpalValue>,
    pub expression: &'v AstExpression,
}

// TODO: I: Empty dyn boxes should not allocate. just make them a VorpalValue.
#[derive(Clone, Debug)]
pub struct VorpalDynBox {
    pub type_id: usize,
    pub object: VorpalValue,
    pub table: HashMap<String, InstObjectFunctionSignature>,
}

pub enum VorpalAwaitable<'v> {
    Incomplete {
        stack_rev: Vec<VorpalControl<'v>>,
        state: VorpalControlState<'v>,
        variables: HashMap<VariableId, VorpalValue>,
    },
    Evaluating,
    Complete(VorpalValue),
}

pub struct VorpalHeap<'v> {
    thread_schedule: VecDeque<ThreadId>,
    ready_threads: HashMap<ThreadId, VorpalThread<'v>>,
    blocked_threads: HashMap<ThreadId, VorpalThread<'v>>,

    pub global_variables: HashMap<ModuleRef, VorpalValue>,
    pub extern_fns: HashMap<ModuleRef, VorpalExternFn>,

    heap_structs: HashMap<VorpalHeapPointer, Vec<VorpalValue>>,
    dyn_boxes: HashMap<VorpalDynPointer, VorpalDynBox>,
    closures: HashMap<VorpalClosurePointer, VorpalClosure<'v>>,
    awaitables: HashMap<VorpalAwaitablePointer, VorpalAwaitable<'v>>,
}

impl<'v> VorpalHeap<'v> {
    pub fn new(extern_fns: HashMap<ModuleRef, VorpalExternFn>) -> VorpalHeap<'v> {
        VorpalHeap {
            thread_schedule: VecDeque::new(),
            blocked_threads: hashmap! {},
            ready_threads: hashmap! {},
            global_variables: hashmap! {},
            heap_structs: hashmap! {},
            dyn_boxes: hashmap! {},
            closures: hashmap! {},
            awaitables: hashmap! {},
            extern_fns,
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

    pub fn get_object_idx(&mut self, v: VorpalValue, idx: usize) -> VResult<&mut VorpalValue> {
        if let VorpalValue::HeapCollection { id, offset, limit } = v {
            let adj_idx = idx + offset;

            if adj_idx >= limit {
                return vorpal_panic!(
                    "Array deref out of bounds, idx={}, len={}",
                    idx,
                    limit - offset,
                );
            }

            Ok(self
                .heap_structs
                .get_mut(&id)
                .expect("ICE: null pointer")
                .get_mut(idx)
                .expect("ICE: Object/Array is too small to get indexed value"))
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

    pub fn allocate_awaitable(
        &mut self,
        expression: &'v AstExpression,
        variables: HashMap<VariableId, VorpalValue>,
    ) -> VorpalValue {
        let id = VorpalAwaitablePointer(VORPAL_POINTER_COUNTER.fetch_add(1, Ordering::Relaxed));

        self.awaitables.insert(id, VorpalAwaitable::Incomplete {
            stack_rev: vec![VorpalControl::Async(id)],
            variables,
            state: VorpalControlState::Expression(expression),
        });

        VorpalValue::Awaitable(id)
    }

    pub fn get_awaitable(&mut self, a: VorpalAwaitablePointer) -> &mut VorpalAwaitable<'v> {
        self.awaitables.get_mut(&a).unwrap()
    }

    pub fn save_awaitable_incomplete(
        &mut self,
        id: VorpalAwaitablePointer,
        stack_rev: Vec<VorpalControl<'v>>,
        state: VorpalControlState<'v>,
        variables: HashMap<VariableId, VorpalValue>,
    ) {
        *self.get_awaitable(id) = VorpalAwaitable::Incomplete {
            stack_rev,
            state,
            variables,
        }
    }

    pub fn save_awaitable_complete(&mut self, id: VorpalAwaitablePointer, value: VorpalValue) {
        *self.get_awaitable(id) = VorpalAwaitable::Complete(value)
    }

    pub fn gc(&mut self, current_thread: &VorpalThread) {
        let mut gc = VorpalGc::new();

        self.visit_thread(&mut gc, current_thread);

        for t in self
            .blocked_threads
            .values()
            .chain(self.ready_threads.values())
        {
            self.visit_thread(&mut gc, t);
        }

        for g in self.global_variables.values() {
            self.visit(&mut gc, g);
        }

        debug!(
            "Removed structs: {:?}",
            self.heap_structs
                .keys()
                .filter(|k| !gc.heap_structs.contains(k))
                .collect::<Vec<_>>()
        );
        debug!(
            "Removed dyn boxes: {:?}",
            self.dyn_boxes
                .keys()
                .filter(|k| !gc.dyn_boxes.contains(k))
                .collect::<Vec<_>>()
        );
        debug!(
            "Removed closures: {:?}",
            self.closures
                .keys()
                .filter(|k| !gc.closures.contains(k))
                .collect::<Vec<_>>()
        );
        debug!(
            "Removed awaitables: {:?}",
            self.awaitables
                .keys()
                .filter(|k| !gc.awaitables.contains(k))
                .collect::<Vec<_>>()
        );

        self.heap_structs.retain(|k, _| gc.heap_structs.contains(k));
        self.dyn_boxes.retain(|k, _| gc.dyn_boxes.contains(k));
        self.closures.retain(|k, _| gc.closures.contains(k));
        self.awaitables.retain(|k, _| gc.awaitables.contains(k));
    }

    fn visit(&self, gc: &mut VorpalGc, v: &VorpalValue) {
        match v {
            VorpalValue::Int(_)
            | VorpalValue::Float(_)
            | VorpalValue::String(_)
            | VorpalValue::GlobalFn(_)
            | VorpalValue::Undefined => {},
            VorpalValue::ValueCollection { values } =>
                for v in values {
                    self.visit(gc, v);
                },
            VorpalValue::EnumVariant { values, .. } =>
                for v in values {
                    self.visit(gc, v);
                },
            VorpalValue::HeapCollection { id, .. } =>
                if gc.heap_structs.insert(*id) {
                    for v in &self.heap_structs[id] {
                        self.visit(gc, v);
                    }
                },
            VorpalValue::Closure(id) =>
                if gc.closures.insert(*id) {
                    let VorpalClosure { captured, .. } = &self.closures[id];

                    for v in captured.values() {
                        self.visit(gc, v);
                    }
                },
            VorpalValue::DynamicBox(id) =>
                if gc.dyn_boxes.insert(*id) {
                    let VorpalDynBox { object, .. } = &self.dyn_boxes[id];
                    self.visit(gc, object);
                },
            VorpalValue::Awaitable(id) =>
                if gc.awaitables.insert(*id) {
                    match &self.awaitables[id] {
                        VorpalAwaitable::Complete(v) => {
                            self.visit(gc, v);
                        },
                        VorpalAwaitable::Incomplete {
                            stack_rev,
                            state,
                            variables,
                        } => {
                            self.visit_state(gc, state);

                            for s in stack_rev {
                                self.visit_control(gc, s);
                            }

                            for v in variables.values() {
                                self.visit(gc, v);
                            }
                        },
                        VorpalAwaitable::Evaluating => {},
                    }
                },
        }
    }

    fn visit_thread(&self, gc: &mut VorpalGc, t: &VorpalThread) {
        self.visit(gc, &t.thread_object);

        for c in &t.control {
            self.visit_control(gc, c);
        }

        for vs in &t.variables {
            for v in vs.values() {
                self.visit(gc, v);
            }
        }
    }

    fn visit_control(&self, gc: &mut VorpalGc, c: &VorpalControl<'v>) {
        match c {
            VorpalControl::Block { .. } => {},
            VorpalControl::CallBody => {},
            VorpalControl::ApplyToLval(_) => {},
            VorpalControl::ApplyToLet(_) => {},
            VorpalControl::TupleAccess(_) => {},
            VorpalControl::ObjectAccess(_) => {},
            VorpalControl::If { .. } => {},
            VorpalControl::Match { .. } => {},
            VorpalControl::Break(_) => {},
            VorpalControl::Return => {},
            VorpalControl::AwaitPrePoll { .. } => {},
            VorpalControl::AwaitPostPoll { .. } => {},
            VorpalControl::ParkedState(s) => {
                self.visit_state(gc, s);
            },
            VorpalControl::CallFn { args, .. } =>
                for v in args {
                    self.visit(gc, v);
                },
            VorpalControl::CallObjFn { args, .. } =>
                for v in args {
                    self.visit(gc, v);
                },
            VorpalControl::ApplyRval { rval, .. } => {
                self.visit(gc, rval);
            },
            VorpalControl::ApplyRvalToObject { rval, .. } => {
                self.visit(gc, rval);
            },
            VorpalControl::Tuple { values, .. } =>
                for v in values {
                    self.visit(gc, v);
                },
            VorpalControl::Array { values, .. } =>
                for v in values {
                    self.visit(gc, v);
                },
            VorpalControl::Object { values, .. } =>
                for v in values {
                    self.visit(gc, v);
                },
            VorpalControl::Enum { values, .. } =>
                for v in values {
                    self.visit(gc, v);
                },
            VorpalControl::PreWhile { exit_value, .. } =>
                if let Some(v) = exit_value {
                    self.visit(gc, v);
                },
            VorpalControl::PostWhile { exit_value, .. } =>
                if let Some(v) = exit_value {
                    self.visit(gc, v);
                },
            VorpalControl::Instruction { values, .. } =>
                for v in values {
                    match v {
                        super::thread::VorpalInstructionArgument::Value(v) => {
                            self.visit(gc, v);
                        },
                        super::thread::VorpalInstructionArgument::Type(_)
                        | super::thread::VorpalInstructionArgument::Anonymous(_) => {},
                    }
                },
            VorpalControl::Async(a) => {
                self.visit(gc, &VorpalValue::Awaitable(*a));
            },
        }
    }

    fn visit_state(&self, gc: &mut VorpalGc, s: &VorpalControlState<'v>) {
        match s {
            VorpalControlState::Initial
            | VorpalControlState::LvalExpression(_)
            | VorpalControlState::Statement(_)
            | VorpalControlState::Expression(_) => {},
            VorpalControlState::Value(v) => {
                self.visit(gc, v);
            },
        }
    }
}

impl<'v> VorpalHeap<'v> {
    pub fn new_thread(&mut self, exit_type_id: usize) -> VorpalThread<'v> {
        let thread_id = VORPAL_THREAD_COUNTER.fetch_add(1, Ordering::Relaxed);

        VorpalThread {
            thread_id: ThreadId(thread_id),
            control: vec![],
            variables: vec![],
            blocking: HashSet::new(),
            blocked_on: HashSet::new(),
            exit_type_id,
            start: Instant::now(),
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

    pub fn num_ready_threads(&self) -> usize {
        assert_eq!(self.ready_threads.len(), self.thread_schedule.len());

        self.ready_threads.len()
    }

    pub fn num_all_threads(&self, current: bool) -> usize {
        self.ready_threads.len() + self.blocked_threads.len() + (if current { 1 } else { 0 })
    }

    pub fn get_other_thread_ids(&self) -> Vec<ThreadId> {
        let mut ids = vec![];
        ids.extend(self.ready_threads.keys().cloned());
        ids.extend(self.blocked_threads.keys().cloned());
        ids
    }

    pub fn get_thread<'t>(
        &'t mut self,
        thread_id: ThreadId,
        current_thread: &'t mut VorpalThread<'v>,
    ) -> VResult<&'t mut VorpalThread<'v>> {
        if let Some(other_thread) = self.ready_threads.get_mut(&thread_id) {
            Ok(other_thread)
        } else if let Some(other_thread) = self.blocked_threads.get_mut(&thread_id) {
            Ok(other_thread)
        } else if current_thread.thread_id == thread_id {
            Ok(current_thread)
        } else {
            vorpal_panic!(
                "ICE: Cannot block thread {} on thread {} which doesn't exist",
                thread_id,
                thread_id
            )
        }
    }

    pub fn pop_thread(&mut self) -> VResult<VorpalThread<'v>> {
        assert_eq!(self.ready_threads.len(), self.thread_schedule.len());

        if let Some(next_id) = self.thread_schedule.pop_front() {
            let thread = self
                .ready_threads
                .remove(&next_id)
                .expect("ICE: scheduled thread not in ready_threads");
            Ok(thread)
        } else {
            vorpal_panic!("No new threads to schedule... Reached a deadlock!")
        }
    }

    pub fn push_thread(&mut self, t: VorpalThread<'v>, front: bool) {
        assert_eq!(self.ready_threads.len(), self.thread_schedule.len());

        let thread_id = t.thread_id;
        self.ready_threads.insert(thread_id, t);

        if front {
            self.thread_schedule.push_front(thread_id);
        } else {
            self.thread_schedule.push_back(thread_id);
        }
    }

    pub fn signal_thread(&mut self, my_id: ThreadId, other_id: ThreadId) {
        assert_ne!(my_id, other_id);

        if let Some(other_thread) = self.blocked_threads.get_mut(&other_id) {
            other_thread.blocked_on.remove(&my_id);

            if other_thread.blocked_on.is_empty() {
                let other_thread = self.blocked_threads.remove(&other_id).unwrap();
                self.push_thread(other_thread, false);
            }
        } else {
            unreachable!("ICE: can only signal blocked threads");
        }
    }

    pub fn block_thread(
        &mut self,
        mut blocked_thread: VorpalThread<'v>,
        other_ids: &[ThreadId],
        current_thread: &mut VorpalThread<'v>,
    ) -> VResult<()> {
        let thread_id = blocked_thread.thread_id;

        if other_ids.len() == 0 && blocked_thread.blocked_on.len() == 0 {
            return vorpal_panic!("ICE: Cannot block thread {} on 0 threads", thread_id);
        }

        for other_id in other_ids {
            let other_id = *other_id;

            if blocked_thread.thread_id == other_id {
                return vorpal_panic!("ICE: Cannot block thread {} on itself!", other_id);
            }

            blocked_thread.blocked_on.insert(other_id);
            self.get_thread(other_id, current_thread)?
                .blocking
                .insert(thread_id);
        }

        self.blocked_threads.insert(thread_id, blocked_thread);

        Ok(())
    }

    pub fn is_thread_live(&self, other_id: ThreadId) -> bool {
        self.ready_threads.contains_key(&other_id) || self.blocked_threads.contains_key(&other_id)
    }
}

struct VorpalGc {
    heap_structs: HashSet<VorpalHeapPointer>,
    dyn_boxes: HashSet<VorpalDynPointer>,
    closures: HashSet<VorpalClosurePointer>,
    awaitables: HashSet<VorpalAwaitablePointer>,
}

impl VorpalGc {
    fn new() -> VorpalGc {
        VorpalGc {
            heap_structs: HashSet::new(),
            dyn_boxes: HashSet::new(),
            closures: HashSet::new(),
            awaitables: HashSet::new(),
        }
    }
}
