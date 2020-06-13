use crate::{
    ana::represent::AnalyzedProgram,
    ast::{
        AstBlock, AstExpression, AstExpressionData, AstFunction, AstGlobalVariable, AstLiteral,
        AstMatchPattern, AstMatchPatternData, AstNamedVariable, AstObject, AstObjectFunction,
        AstStatement, AstTraitType, AstTraitTypeWithAssocs, AstType, InstructionArgument,
        InstructionOutput, ModuleRef, VariableId,
    },
    inst::{
        decorate::decorate_dynamic_fn, InstEnumRepresentation, InstEnumSignature,
        InstFunctionSignature, InstObjectFunctionSignature, InstObjectSignature,
        InstantiatedProgram,
    },
    util::{PResult, Span, ZipExact},
    vs::{heap::*, thread::*, value::*},
};
use exported::{get_extern_fns, VorpalExternFn};
use itertools::Itertools;
use std::{collections::HashMap, rc::Rc, time::Instant};

#[macro_use]
mod thread;
mod exported;
mod heap;
mod value;

const MAIN_THREAD_ID: ThreadId = ThreadId(0);
const THREAD_QUANTUM: u128 = 50;

struct VorpalSword {
    pub main_fn: ModuleRef,

    pub program_fns: HashMap<InstFunctionSignature, AstFunction>,
    pub program_object_fns: HashMap<InstObjectFunctionSignature, AstObjectFunction>,
    pub program_objects: HashMap<InstObjectSignature, AstObject>,
    pub program_enums: HashMap<InstEnumSignature, InstEnumRepresentation>,
    pub program_globals: HashMap<ModuleRef, AstGlobalVariable>,

    /// Precomputed associations to make calculating dynamic v-tables easier
    pub program_object_tables:
        HashMap<(AstType, AstTraitType), HashMap<String, InstObjectFunctionSignature>>,
    pub type_ids: HashMap<AstType, usize>,
}

pub fn evaluate(
    instantiated_program: InstantiatedProgram,
    analyzed_program: Rc<AnalyzedProgram>,
) -> PResult<()> {
    let InstantiatedProgram {
        main_fn,

        instantiated_fns,
        instantiated_object_fns,
        instantiated_objects,
        instantiated_enums,
        instantiated_globals,

        instantiated_types,
        ..
    } = instantiated_program;

    let mut program_object_tables: HashMap<_, HashMap<_, _>> = hashmap! {};

    for (sig, fun) in &instantiated_object_fns {
        if !is_dyn_dispatchable(fun) {
            continue;
        }

        let InstObjectFunctionSignature(ty, trt, name, _) = sig;

        if let Some(trt) = trt {
            let decorated_name = decorate_dynamic_fn(trt, name)?;

            program_object_tables
                .entry((ty.clone(), trt.clone()))
                .or_default()
                .insert(decorated_name, sig.clone());
        }
    }

    let vs = VorpalSword {
        main_fn,

        program_fns: instantiated_fns,
        program_object_fns: instantiated_object_fns,
        program_objects: instantiated_objects,
        program_enums: instantiated_enums,
        program_globals: instantiated_globals,

        program_object_tables,
        type_ids: instantiated_types
            .into_iter()
            .enumerate()
            .map(|(i, t)| (t, i))
            .collect(),
    };

    let extern_fns = get_extern_fns(&analyzed_program)?;

    let exit_code = vs.run(extern_fns);

    match exit_code {
        Ok(exit_code) => {
            info!("Exited normally with code `{}`", exit_code);
            Ok(())
        },
        Err(VError::Exit(exit_code)) => {
            info!("Exited with `exit({})`", exit_code);
            Ok(())
        },
        Err(VError::Panic { error, .. }) => Err(error),
    }
}

// TODO: I: Put #inline on the right functions (basically every heap fn, every
// leaf fn `do_*`)

impl VorpalSword {
    fn run(&self, extern_fns: HashMap<ModuleRef, VorpalExternFn>) -> VResult<i64> {
        let mut heap = VorpalHeap::new(extern_fns);
        // Allocate the main_thread first, so we can make sure it has thread_id == 0.
        let mut main_thread = heap.new_thread(self.type_ids[&AstType::Int]);
        assert_eq!(main_thread.thread_id, MAIN_THREAD_ID);

        for (name, var) in &self.program_globals {
            let mut global_mini_thread = heap.new_thread(self.type_ids[&var.ty]);
            global_mini_thread.control.push(VorpalControl::ParkedState(
                VorpalControlState::Expression(&var.init),
            ));

            match self.run_thread(&mut heap, &mut global_mini_thread)? {
                VorpalThreadState::Complete(e) => {
                    let exit_type_id = global_mini_thread.exit_type_id;
                    let dyn_exit_value = self.make_plain_dynamic_box(
                        &mut heap,
                        &mut global_mini_thread,
                        e.clone(),
                        exit_type_id,
                    )?;
                    *heap.get_object_idx(global_mini_thread.thread_object, 1)? =
                        VorpalValue::EnumVariant {
                            variant: "Some".to_string(),
                            values: vec![dyn_exit_value],
                        };

                    heap.global_variables.insert(name.clone(), e);
                },
                VorpalThreadState::Incomplete => {
                    return vorpal_panic_at!(
                        var.name_span,
                        "Thread was preempted during global initialization ({}). Multithreading \
                         is not allowed before the main function starts!",
                        name.full_name()
                    );
                },
            }
        }

        let main_fun = InstFunctionSignature(self.main_fn.clone(), vec![]);

        // Initialize the state with a single fn call to main.
        let state = self.do_fn_call(&mut heap, &mut main_thread, main_fun, vec![])?;
        main_thread.control.push(VorpalControl::ParkedState(state));

        let mut current_thread = main_thread;
        loop {
            match self.run_thread(&mut heap, &mut current_thread)? {
                VorpalThreadState::Complete(exit_code)
                    if current_thread.thread_id == MAIN_THREAD_ID =>
                    if let VorpalValue::Int(exit_code) = exit_code {
                        debug!("Main thread exited!");
                        break Ok(exit_code);
                    } else {
                        unreachable!(
                            "ICE: main thread did not return an Int, but instead: `{:?}`!",
                            exit_code
                        )
                    },
                VorpalThreadState::Complete(exit_value) => {
                    let exit_type_id = current_thread.exit_type_id;
                    let dyn_exit_value = self.make_plain_dynamic_box(
                        &mut heap,
                        &mut current_thread,
                        exit_value,
                        exit_type_id,
                    )?;
                    *heap.get_object_idx(current_thread.thread_object, 1)? =
                        VorpalValue::EnumVariant {
                            variant: "Some".to_string(),
                            values: vec![dyn_exit_value],
                        };

                    let old_thread_id = current_thread.thread_id;

                    // Signal all blocked threads
                    for blocked_id in current_thread.blocking {
                        heap.signal_thread(old_thread_id, blocked_id);
                    }

                    // Switch processes!
                    current_thread = heap.pop_thread()?;
                    debug!(
                        "Switching from thread {} -> {} (thread completed)",
                        old_thread_id, current_thread.thread_id
                    );
                },
                VorpalThreadState::Incomplete =>
                    if heap.num_ready_threads() > 0 {
                        let new_thread = heap.pop_thread()?;
                        debug!(
                            "Switching from thread {} -> {}",
                            current_thread.thread_id, new_thread.thread_id
                        );
                        heap.push_thread(current_thread, false);
                        current_thread = new_thread;
                    },
            }
        }
    }

    fn run_thread<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
    ) -> VResult<VorpalThreadState> {
        thread.start = Instant::now();
        let mut cstate = VorpalControlState::Initial;

        // The state machine can continue to progress until we have 1 value, with no
        // control state.
        while !thread.control.is_empty() || !cstate.is_value() {
            if thread.start.elapsed().as_millis() > THREAD_QUANTUM {
                if heap.num_ready_threads() > 0 {
                    thread.control.push(VorpalControl::ParkedState(cstate));
                    return Ok(VorpalThreadState::Incomplete);
                } else {
                    thread.start = Instant::now();
                }
            }

            let new_cstate = match cstate {
                VorpalControlState::Initial => self.apply_initial(heap, thread),
                VorpalControlState::Expression(e) => self.apply_expression(heap, thread, e),
                VorpalControlState::LvalExpression(l) =>
                    self.apply_lval_expression(heap, thread, l),
                VorpalControlState::Statement(s) => self.apply_statement(heap, thread, s),
                VorpalControlState::Value(v) => self.apply_value(heap, thread, v),
            };

            match new_cstate {
                Ok(new_cstate) => cstate = new_cstate,
                Err(err) => {
                    return Err(err /* TODO: VS: err.add_stacktrace(thread.control) */);
                },
            }
        }

        Ok(VorpalThreadState::Complete(cstate.unwrap_value()))
    }

    fn apply_initial<'v>(
        &'v self,
        _: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
    ) -> VResult<VorpalControlState<'v>> {
        assert!(
            !thread.control.is_empty(),
            "ICE: cannot apply initial state to empty control stack"
        );

        Ok(match thread.control.pop().unwrap() {
            VorpalControl::ParkedState(s) => s,
            f => unreachable!("ICE: Can only unpark a parked state, got `{:?}`", f),
        })
    }

    fn apply_expression<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        expr: &'v AstExpression,
    ) -> VResult<VorpalControlState<'v>> {
        Ok(match &expr.data {
            AstExpressionData::SelfRef
            | AstExpressionData::Unimplemented
            | AstExpressionData::ExprCall { .. }
            | AstExpressionData::NamedEnum { .. }
            | AstExpressionData::PlainEnum { .. }
            | AstExpressionData::AllocateArray { .. }
            | AstExpressionData::As { .. }
            | AstExpressionData::For { .. }
            | AstExpressionData::Assert { .. }
            | AstExpressionData::ArrayAccess { .. }
            | AstExpressionData::ObjectCall { .. }
            | AstExpressionData::Not(_)
            | AstExpressionData::Negate(_)
            | AstExpressionData::BinOp { .. }
            | AstExpressionData::Throw { .. } => unreachable!(
                "ICE: Unexpected expression kind: {:#?}..... should've been desugared.",
                expr.data
            ),

            AstExpressionData::Literal(AstLiteral::True) =>
                VorpalControlState::Value(VorpalValue::Int(1)),
            AstExpressionData::Literal(AstLiteral::False) =>
                VorpalControlState::Value(VorpalValue::Int(0)),
            AstExpressionData::Literal(AstLiteral::Char(chr)) =>
                VorpalControlState::Value(VorpalValue::Int(*chr as i64)),
            AstExpressionData::Literal(AstLiteral::Int(int)) =>
                VorpalControlState::Value(VorpalValue::Int(int.parse().unwrap())),
            AstExpressionData::Literal(AstLiteral::Float(float)) =>
                VorpalControlState::Value(VorpalValue::Float(float.parse().unwrap())),
            AstExpressionData::Literal(AstLiteral::String(string)) =>
                VorpalControlState::Value(VorpalValue::String(string.clone())), /* TODO: I: Should I heap allocate strings? */

            AstExpressionData::Identifier { variable_id, name } =>
                if let Some(value) = thread
                    .variables
                    .last()
                    .unwrap()
                    .get(variable_id.as_ref().unwrap())
                {
                    VorpalControlState::Value(value.clone())
                } else {
                    unreachable!(
                        "Variable `{}` ({:?}) is undefined -- env: `{:#?}`",
                        name, variable_id, thread.variables
                    )
                },
            AstExpressionData::GlobalVariable { name } =>
                VorpalControlState::Value(heap.get_global(expr.span, name)?.clone()),
            AstExpressionData::GlobalFn { name, generics } => VorpalControlState::Value(
                VorpalValue::GlobalFn(InstFunctionSignature(name.clone(), generics.clone())),
            ),

            AstExpressionData::Tuple { values } =>
                if values.is_empty() {
                    VorpalControlState::Value(VorpalValue::unit())
                } else {
                    let uneval_rev = values[1..].iter().rev().collect();
                    let first = &values[0];

                    thread.control.push(VorpalControl::Tuple {
                        uneval_rev,
                        values: vec![],
                    });
                    VorpalControlState::Expression(first)
                },

            AstExpressionData::ArrayLiteral { elements } =>
                if elements.is_empty() {
                    VorpalControlState::Value(heap.allocate_heap_object(vec![]))
                } else {
                    let uneval_rev = elements[1..].iter().rev().collect();
                    let first = &elements[0];

                    thread.control.push(VorpalControl::Array {
                        uneval_rev,
                        values: vec![],
                    });
                    VorpalControlState::Expression(first)
                },

            AstExpressionData::Closure {
                params,
                expr,
                captured,
                ..
            } => {
                let scope = thread.variables.last().unwrap();
                let mut env = hashmap! {};

                for (old, new) in captured.as_ref().unwrap() {
                    env.insert(
                        new.id,
                        scope
                            .get(&old.id)
                            .expect("ICE: variable is undefined")
                            .clone(),
                    );
                }

                VorpalControlState::Value(heap.allocate_closure(
                    params.iter().collect(),
                    env,
                    expr.as_ref(),
                ))
            },

            AstExpressionData::Async { expr, captured, .. } => {
                let scope = thread.variables.last().unwrap();
                let mut env = hashmap! {};

                for (old, new) in captured.as_ref().unwrap() {
                    env.insert(
                        new.id,
                        scope
                            .get(&old.id)
                            .expect("ICE: variable is undefined")
                            .clone(),
                    );
                }

                VorpalControlState::Value(heap.allocate_awaitable(expr.as_ref(), env))
            },

            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => {
                // TODO(borrow_key): We could really do without these clones.
                let fun = InstFunctionSignature(fn_name.clone(), generics.clone());

                if args.is_empty() {
                    self.do_fn_call(heap, thread, fun, vec![])?
                } else {
                    let uneval_rev = args[1..].iter().rev().collect();
                    let expr = &args[0];
                    thread.control.push(VorpalControl::CallFn {
                        fun,
                        uneval_rev,
                        args: vec![],
                    });
                    VorpalControlState::Expression(expr)
                }
            },

            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                ..
            } => {
                // TODO(borrow_key): We could really do without these clones.
                let fun = InstObjectFunctionSignature(
                    call_type.clone(),
                    associated_trait.as_ref().map(|t| t.trt.clone()),
                    fn_name.clone(),
                    fn_generics.clone(),
                );

                if args.is_empty() {
                    self.do_obj_call(heap, thread, fun, vec![])?
                } else {
                    let uneval_rev = args[1..].iter().rev().collect();
                    let expr = &args[0];
                    thread.control.push(VorpalControl::CallObjFn {
                        fun,
                        uneval_rev,
                        args: vec![],
                    });
                    VorpalControlState::Expression(expr)
                }
            },

            AstExpressionData::TupleAccess { accessible, idx } => {
                if let Some(VorpalControl::TupleAccess(indices)) = thread.control.last_mut() {
                    indices.push(*idx);
                } else {
                    thread.control.push(VorpalControl::TupleAccess(vec![*idx]));
                }

                VorpalControlState::Expression(accessible.as_ref())
            },

            AstExpressionData::ObjectAccess {
                object, mem_idx, ..
            } => {
                thread
                    .control
                    .push(VorpalControl::ObjectAccess(mem_idx.unwrap()));
                VorpalControlState::Expression(object.as_ref())
            },

            AstExpressionData::AllocateObject {
                children,
                children_idxes,
                ..
            } => {
                if children.is_empty() {
                    // TODO: I: Let's allocate a singleton empty object
                    VorpalControlState::Value(heap.allocate_heap_object(vec![]))
                } else {
                    let children_idxes = children_idxes.as_ref().unwrap();
                    let mut placeholders = vec![Option::None; children.len()];

                    for (name, expr) in children {
                        placeholders[children_idxes[name]] = Some(expr);
                    }

                    let uneval_rev = placeholders[1..]
                        .into_iter()
                        .rev()
                        .map(|o| o.unwrap())
                        .collect();
                    let expr = placeholders[0].unwrap();
                    thread.control.push(VorpalControl::Object {
                        uneval_rev,
                        values: vec![],
                    });
                    VorpalControlState::Expression(expr)
                }
            },

            AstExpressionData::Assign { lhs, rhs } => {
                thread
                    .control
                    .push(VorpalControl::ApplyToLval(lhs.as_ref()));
                VorpalControlState::Expression(rhs.as_ref())
            },

            AstExpressionData::Block { block } => self.do_block(heap, thread, block)?,

            AstExpressionData::If {
                condition,
                block,
                else_block,
            } => {
                thread.control.push(VorpalControl::If { block, else_block });
                VorpalControlState::Expression(condition.as_ref())
            },

            AstExpressionData::Match {
                expression,
                branches,
            } => {
                thread.control.push(VorpalControl::Match {
                    span: expr.span,
                    branches,
                });
                VorpalControlState::Expression(expression.as_ref())
            },

            AstExpressionData::While {
                id,
                condition,
                block,
                else_block,
                ..
            } => {
                thread.control.push(VorpalControl::PreWhile {
                    id: *id,
                    condition: condition.as_ref(),
                    block,
                    else_block,
                    exit_value: None,
                });
                VorpalControlState::Expression(condition.as_ref())
            },

            AstExpressionData::PositionalEnum {
                variant, children, ..
            } =>
                if children.is_empty() {
                    VorpalControlState::Value(VorpalValue::EnumVariant {
                        variant: variant.clone(),
                        values: vec![],
                    })
                } else {
                    let uneval_rev = children[1..].iter().rev().collect();
                    let expr = &children[0];
                    thread.control.push(VorpalControl::Enum {
                        variant: variant.clone(),
                        uneval_rev,
                        values: vec![],
                    });
                    VorpalControlState::Expression(expr)
                },

            AstExpressionData::Break { id, value, .. } => {
                thread.control.push(VorpalControl::Break(id.unwrap()));
                VorpalControlState::Expression(value.as_ref())
            },

            AstExpressionData::Continue { id, .. } => {
                loop {
                    match thread
                        .control
                        .last()
                        .expect("ICE: reached end of stack before while loop continue")
                    {
                        VorpalControl::PreWhile {
                            id: other_id,
                            condition,
                            ..
                        } if id.unwrap() == *other_id => {
                            break VorpalControlState::Expression(*condition);
                        },
                        VorpalControl::CallBody =>
                            unreachable!("ICE: should not be able to continue across functions..."),
                        _ => {
                            // Pop the frames until we get to our loop.
                            thread.control.pop();
                        },
                    }
                }
            },
            AstExpressionData::Return { value } => {
                thread.control.push(VorpalControl::Return);
                VorpalControlState::Expression(value.as_ref())
            },

            AstExpressionData::Await {
                value,
                associated_trait,
                ..
            } => {
                let fun = InstObjectFunctionSignature(
                    value.ty.clone(),
                    Some(associated_trait.as_ref().unwrap().trt.clone()),
                    "poll".to_string(),
                    vec![],
                );

                thread.control.push(VorpalControl::AwaitPrePoll {
                    span: expr.span,
                    fun,
                });
                VorpalControlState::Expression(value.as_ref())
            },

            AstExpressionData::ConditionalCompilation { branches } => {
                let block = &branches["vorpal_sword"];
                self.do_block(heap, thread, block)?
            },

            AstExpressionData::Instruction {
                instruction,
                arguments,
                output,
            } => {
                let instruction = &instruction;

                if arguments.is_empty() {
                    self.do_instruction(heap, thread, expr.span, instruction, vec![], output)?
                } else {
                    let mut uneval_rev = arguments.iter().rev().collect();
                    let mut values = vec![];

                    match self.find_instruction_arg(&mut uneval_rev, &mut values) {
                        Some(e) => {
                            thread.control.push(VorpalControl::Instruction {
                                instruction,
                                output,
                                uneval_rev,
                                values,
                                span: expr.span,
                            });
                            VorpalControlState::Expression(e)
                        },
                        None => self.do_instruction(
                            heap,
                            thread,
                            expr.span,
                            instruction,
                            values,
                            output,
                        )?,
                    }
                }
            },
        })
    }

    fn apply_lval_expression<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        expr: &'v AstExpression,
    ) -> VResult<VorpalControlState<'v>> {
        assert!(
            !thread.control.is_empty(),
            "ICE: cannot apply lval state to empty control stack"
        );

        Ok(match (thread.control.pop().unwrap(), &expr.data) {
            (
                VorpalControl::ApplyRval { rval, indices_rev },
                AstExpressionData::GlobalVariable { name },
            ) => {
                let mut lval = heap.get_global(expr.span, name)?;

                for idx in indices_rev.into_iter().rev() {
                    lval = lval.get_tuple_idx_mut(idx);
                }

                *lval = rval.clone();
                VorpalControlState::Value(rval)
            },
            (
                VorpalControl::ApplyRval { rval, indices_rev },
                AstExpressionData::Identifier { variable_id, .. },
            ) => {
                let mut lval = thread
                    .variables
                    .last_mut()
                    .unwrap()
                    .get_mut(variable_id.as_ref().unwrap())
                    .unwrap();

                for idx in indices_rev.into_iter().rev() {
                    lval = lval.get_tuple_idx_mut(idx);
                }

                *lval = rval.clone();
                VorpalControlState::Value(rval)
            },
            (
                VorpalControl::ApplyRval {
                    rval,
                    mut indices_rev,
                },
                AstExpressionData::TupleAccess { accessible, idx },
            ) => {
                // TODO: I: Do I want to have rhs <- lhs order of ops?
                indices_rev.push(*idx);
                thread
                    .control
                    .push(VorpalControl::ApplyRval { rval, indices_rev });
                VorpalControlState::LvalExpression(accessible.as_ref())
            },
            (
                VorpalControl::ApplyRval { rval, indices_rev },
                AstExpressionData::ObjectAccess {
                    object, mem_idx, ..
                },
            ) => {
                thread.control.push(VorpalControl::ApplyRvalToObject {
                    rval,
                    mem_idx: mem_idx.unwrap(),
                    indices_rev,
                });
                VorpalControlState::Expression(object.as_ref())
            },
            (VorpalControl::ApplyRval { .. }, e) =>
                unreachable!("ICE: Unable to apply rval to invalid lval `{:#?}`", e),
            _ => unreachable!("ICE: Cannot apply lval to non-ApplyRval state"),
        })
    }

    fn apply_statement<'v>(
        &'v self,
        _: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        stmt: &'v AstStatement,
    ) -> VResult<VorpalControlState<'v>> {
        Ok(match stmt {
            AstStatement::Expression { expression } => VorpalControlState::Expression(expression),
            AstStatement::Let { pattern, value } => {
                thread.control.push(VorpalControl::ApplyToLet(pattern));
                VorpalControlState::Expression(value)
            },
        })
    }

    fn apply_value<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        value: VorpalValue,
    ) -> VResult<VorpalControlState<'v>> {
        assert!(
            !thread.control.is_empty(),
            "ICE: control loop should have exited before applying value to empty control stack"
        );

        Ok(match thread.control.pop().unwrap() {
            s @ VorpalControl::ParkedState(_) | s @ VorpalControl::ApplyRval { .. } =>
                unreachable!(
                    "ICE: Cannot apply value `{:?}` to stack topped by `{:?}`",
                    value, s
                ),

            VorpalControl::Block {
                mut uneval_rev,
                expr,
            } =>
                if let Some(stmt) = uneval_rev.pop() {
                    thread
                        .control
                        .push(VorpalControl::Block { uneval_rev, expr });
                    VorpalControlState::Statement(stmt)
                } else {
                    VorpalControlState::Expression(expr)
                },
            VorpalControl::CallFn {
                fun,
                mut uneval_rev,
                mut args,
            } => {
                args.push(value);

                if let Some(expr) = uneval_rev.pop() {
                    thread.control.push(VorpalControl::CallFn {
                        fun,
                        uneval_rev,
                        args,
                    });
                    VorpalControlState::Expression(expr)
                } else {
                    self.do_fn_call(heap, thread, fun, args)?
                }
            },
            VorpalControl::CallObjFn {
                fun,
                mut uneval_rev,
                mut args,
            } => {
                args.push(value);

                if let Some(expr) = uneval_rev.pop() {
                    thread.control.push(VorpalControl::CallObjFn {
                        fun,
                        uneval_rev,
                        args,
                    });
                    VorpalControlState::Expression(expr)
                } else {
                    self.do_obj_call(heap, thread, fun, args)?
                }
            },
            VorpalControl::CallBody => {
                thread.variables.pop();
                VorpalControlState::Value(value)
            },
            VorpalControl::Tuple {
                mut uneval_rev,
                mut values,
            } => {
                values.push(value);
                if let Some(expr) = uneval_rev.pop() {
                    thread
                        .control
                        .push(VorpalControl::Tuple { uneval_rev, values });
                    VorpalControlState::Expression(expr)
                } else {
                    VorpalControlState::Value(VorpalValue::ValueCollection { values })
                }
            },
            VorpalControl::Array {
                mut uneval_rev,
                mut values,
            } => {
                values.push(value);
                if let Some(expr) = uneval_rev.pop() {
                    thread
                        .control
                        .push(VorpalControl::Array { uneval_rev, values });
                    VorpalControlState::Expression(expr)
                } else {
                    VorpalControlState::Value(heap.allocate_heap_object(values))
                }
            },
            VorpalControl::Object {
                mut uneval_rev,
                mut values,
            } => {
                values.push(value);
                if let Some(expr) = uneval_rev.pop() {
                    thread
                        .control
                        .push(VorpalControl::Object { uneval_rev, values });
                    VorpalControlState::Expression(expr)
                } else {
                    VorpalControlState::Value(heap.allocate_heap_object(values))
                }
            },
            VorpalControl::Enum {
                variant,
                mut uneval_rev,
                mut values,
            } => {
                values.push(value);
                if let Some(expr) = uneval_rev.pop() {
                    thread.control.push(VorpalControl::Enum {
                        variant,
                        uneval_rev,
                        values,
                    });
                    VorpalControlState::Expression(expr)
                } else {
                    VorpalControlState::Value(VorpalValue::EnumVariant { variant, values })
                }
            },
            VorpalControl::TupleAccess(indices) => {
                let mut value = value;
                for idx in indices.into_iter().rev() {
                    value = value.get_tuple_idx(idx);
                }
                VorpalControlState::Value(value)
            },
            VorpalControl::ObjectAccess(idx) =>
                VorpalControlState::Value(heap.get_object_idx(value, idx)?.clone()),
            VorpalControl::If { block, else_block } =>
                if value.as_boolean() {
                    self.do_block(heap, thread, block)?
                } else {
                    self.do_block(heap, thread, else_block)?
                },
            VorpalControl::Match { span, branches } => {
                let mut branch = None;
                for b in branches {
                    if self.match_pattern(heap, thread, &b.pattern, &value)? {
                        branch = Some(&b.expression);
                        break;
                    }
                }

                if let Some(branch) = branch {
                    VorpalControlState::Expression(branch)
                } else {
                    return vorpal_panic_at!(span, "Unsatisfied match pattern! ({:?})", value);
                }
            },
            VorpalControl::PreWhile {
                id,
                condition,
                block,
                else_block,
                exit_value,
            } =>
                if value.as_boolean() {
                    thread.control.push(VorpalControl::PostWhile {
                        id,
                        condition,
                        block,
                        else_block,
                        exit_value,
                    });
                    self.do_block(heap, thread, block)?
                } else if let Some(exit_value) = exit_value {
                    VorpalControlState::Value(exit_value)
                } else {
                    self.do_block(heap, thread, else_block)?
                },
            VorpalControl::PostWhile {
                id,
                condition,
                block,
                else_block,
                ..
            } => {
                thread.control.push(VorpalControl::PreWhile {
                    id,
                    condition,
                    block,
                    else_block,
                    exit_value: Some(value),
                });
                VorpalControlState::Expression(condition)
            },
            VorpalControl::Break(id) => loop {
                match thread
                    .control
                    .pop()
                    .expect("ICE: reached end of stack before return")
                {
                    VorpalControl::PostWhile { id: other_id, .. } if id == other_id => {
                        break VorpalControlState::Value(value);
                    },
                    VorpalControl::CallBody =>
                        unreachable!("ICE: should not be able to break across functions..."),
                    _ => {},
                }
            },
            VorpalControl::Return => {
                loop {
                    // TODO: I: If I disallow nested control flow (e.g. no `return return 1`),
                    // then I should be able to actually do this stack-contraction in
                    // `applyExpression`, instead of here in `applyValue`...
                    // That means that `return f()` is a tail call,
                    // which is _VERY_ compelling.
                    match thread
                        .control
                        .pop()
                        .expect("ICE: reached end of stack before return")
                    {
                        VorpalControl::CallBody => {
                            break VorpalControlState::Value(value);
                        },
                        VorpalControl::Async(id) => {
                            heap.save_awaitable_complete(id, value.clone());
                            break VorpalControlState::Value(VorpalValue::ValueCollection {
                                values: vec![
                                    VorpalValue::EnumVariant {
                                        variant: "Complete".to_string(),
                                        values: vec![value],
                                    },
                                    VorpalValue::Awaitable(id),
                                ],
                            });
                        },
                        _ => {},
                    }
                }
            },

            VorpalControl::AwaitPrePoll { span, fun } => {
                thread.control.push(VorpalControl::AwaitPostPoll {
                    span,
                    fun: fun.clone(),
                });

                if let VorpalValue::Awaitable(id) = value {
                    self.do_await(heap, thread, span, id)?
                } else {
                    self.do_obj_call(heap, thread, fun, vec![value])?
                }
            },

            VorpalControl::AwaitPostPoll { span, fun } => {
                let values = if let VorpalValue::ValueCollection { values } = value {
                    values
                } else {
                    unreachable!("ICE: AwaitPostPoll should take a tuple, got {:?}", value)
                };

                let (poll_value, awaitable) = values.into_iter().tuples().next().unwrap();

                let (variant, variant_values) =
                    if let VorpalValue::EnumVariant { variant, values } = poll_value {
                        (variant, values)
                    } else {
                        unreachable!(
                            "ICE: AwaitPostPoll should have a PollState for its 0th tuple \
                             element, got {:?}",
                            poll_value
                        )
                    };

                match variant.as_str() {
                    "Incomplete" => {
                        let mut stack_rev = vec![VorpalControl::AwaitPrePoll { span, fun }];

                        let id = loop {
                            let frame = thread.control.pop().expect(
                                "ICE: Reached end of thread frame without hitting an Async block",
                            );

                            match frame {
                                VorpalControl::Async(id) => {
                                    stack_rev.push(VorpalControl::Async(id));
                                    break id;
                                },
                                VorpalControl::CallBody => {
                                    unreachable!(
                                        "ICE: Await should not be unwinding stack across call \
                                         frames"
                                    );
                                },
                                frame => {
                                    stack_rev.push(frame);
                                },
                            }
                        };

                        heap.save_awaitable_incomplete(
                            id,
                            stack_rev,
                            VorpalControlState::Value(awaitable),
                            thread
                                .variables
                                .pop()
                                .expect("ICE: Expected a variables stack frame"),
                        );
                        VorpalControlState::Value(VorpalValue::ValueCollection {
                            values: vec![
                                VorpalValue::EnumVariant {
                                    variant: "Incomplete".to_string(),
                                    values: vec![],
                                },
                                VorpalValue::Awaitable(id),
                            ],
                        })
                    },
                    "Complete" => {
                        let value = variant_values
                            .into_iter()
                            .nth(0)
                            .expect("ICE: Expected PollState!Complete to have 1 value");
                        VorpalControlState::Value(value)
                    },
                    other => unreachable!("Uknown PollState variant {}", other),
                }
            },
            VorpalControl::Async(id) => {
                thread.variables.pop().expect(
                    "Expected to pop a variable frame corresponding to the async execution",
                );
                heap.save_awaitable_complete(id, value.clone());
                VorpalControlState::Value(VorpalValue::ValueCollection {
                    values: vec![
                        VorpalValue::EnumVariant {
                            variant: "Complete".to_string(),
                            values: vec![value],
                        },
                        VorpalValue::Awaitable(id),
                    ],
                })
            },

            VorpalControl::ApplyToLval(lval) => {
                thread.control.push(VorpalControl::ApplyRval {
                    rval: value,
                    indices_rev: vec![],
                });
                VorpalControlState::LvalExpression(lval)
            },
            VorpalControl::ApplyToLet(pattern) =>
                if !self.match_pattern(heap, thread, pattern, &value)? {
                    unreachable!("ICE: this pattern should be infallible: `{:#?}`", pattern);
                } else {
                    VorpalControlState::Value(VorpalValue::unit())
                },
            VorpalControl::ApplyRvalToObject {
                rval,
                mem_idx,
                indices_rev,
            } => {
                let mut lval = heap.get_object_idx(value, mem_idx)?;

                for idx in indices_rev.into_iter().rev() {
                    lval = lval.get_tuple_idx_mut(idx);
                }

                *lval = rval.clone();
                VorpalControlState::Value(rval)
            },
            VorpalControl::Instruction {
                instruction,
                output,
                mut uneval_rev,
                mut values,
                span,
            } => {
                values.push(VorpalInstructionArgument::Value(value));

                match self.find_instruction_arg(&mut uneval_rev, &mut values) {
                    Some(e) => {
                        thread.control.push(VorpalControl::Instruction {
                            instruction,
                            output,
                            uneval_rev,
                            values,
                            span,
                        });
                        VorpalControlState::Expression(e)
                    },
                    None => self.do_instruction(heap, thread, span, instruction, values, output)?,
                }
            },
        })
    }

    fn do_block<'v>(
        &'v self,
        _: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        block: &'v AstBlock,
    ) -> VResult<VorpalControlState<'v>> {
        let expr = block.expression.as_ref();
        if block.statements.is_empty() {
            Ok(VorpalControlState::Expression(expr))
        } else {
            let uneval_rev = block.statements[1..].iter().rev().collect();
            let stmt = &block.statements[0];

            thread
                .control
                .push(VorpalControl::Block { uneval_rev, expr });

            Ok(VorpalControlState::Statement(stmt))
        }
    }

    fn do_fn_call<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        fun_sig: InstFunctionSignature,
        args: Vec<VorpalValue>,
    ) -> VResult<VorpalControlState<'v>> {
        let fun = &self.program_fns[&fun_sig];

        if let Some(definition) = &fun.definition {
            self.do_call(heap, thread, &fun.parameter_list, args, definition)
        } else if let Some(definition) = heap.extern_fns.get(&fun.module_ref) {
            let value = definition(heap, thread, &fun_sig.1, args)?;
            Ok(VorpalControlState::Value(value))
        } else {
            unreachable!(
                "ICE: Undefined extern function: `{}`",
                fun.module_ref.full_name()
            );
        }
    }

    fn do_obj_call<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        fun: InstObjectFunctionSignature,
        args: Vec<VorpalValue>,
    ) -> VResult<VorpalControlState<'v>> {
        let fun = &self.program_object_fns[&fun];
        self.do_call(
            heap,
            thread,
            &fun.parameter_list,
            args,
            fun.definition
                .as_ref()
                .expect("ICE: can only dispatch to functions with a definition"),
        )
    }

    fn do_dynamic_call<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        span: Span,
        fn_name: &str,
        mut args: Vec<VorpalValue>,
    ) -> VResult<VorpalControlState<'v>> {
        let id = if let VorpalValue::DynamicBox(id) = &args[0] {
            *id
        } else {
            return vorpal_panic_at!(
                span,
                "can only call dynamic functions with DynamicBox as `self` (first) argument"
            );
        };

        let VorpalDynBox { object, table, .. } = heap.get_dyn_box(id);

        // Set up the fun and plug in the _original_ boxed value as arg0.
        let fun = if let Some(fun) = table.get(fn_name) {
            fun.clone()
        } else {
            return vorpal_panic_at!(
                span,
                "cannot dispatch to function `{}`, function table had: {}",
                fn_name,
                table.keys().join(", ")
            );
        };

        args[0] = object.clone();

        self.do_obj_call(heap, thread, fun, args)
    }

    fn do_value_call<'v, 's>(
        &'v self,
        heap: &'s mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        span: Span,
        mut args: Vec<VorpalValue>,
    ) -> VResult<VorpalControlState<'v>> {
        let fun = args.remove(0);

        match fun {
            VorpalValue::GlobalFn(fun) => self.do_fn_call(heap, thread, fun, args),
            VorpalValue::Closure(c) => {
                let VorpalClosure {
                    parameters,
                    captured,
                    expression,
                } = heap.get_closure(c);
                let parameters: Vec<&'v AstMatchPattern> = parameters.clone();
                let captured = captured.clone();
                let expression: &'v AstExpression = *expression;
                self.do_closure_call(heap, thread, &parameters, captured, args, expression)
            },
            f =>
                return vorpal_panic_at!(
                    span,
                    "cannot call value ({:?}) unless it is a fn-ref or closure",
                    f
                ),
        }
    }

    fn do_call<'v>(
        &'v self,
        _: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        params: &[AstNamedVariable],
        args: Vec<VorpalValue>,
        definition: &'v AstExpression,
    ) -> VResult<VorpalControlState<'v>> {
        let mut variables = hashmap! {};
        for (param, arg) in ZipExact::zip_exact(params, args, "args")
            .expect("ICE: parameter list length does not match args")
        {
            variables.insert(param.id, arg);
        }

        if let Some(VorpalControl::CallBody) = thread.control.last() {
            // Tail call optimization!
            let top = thread.variables.last_mut().unwrap();
            top.clear();
            top.extend(variables);
        } else {
            thread.control.push(VorpalControl::CallBody);
            thread.variables.push(variables);
        }

        Ok(VorpalControlState::Expression(definition))
    }

    fn do_closure_call<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        params: &[&'v AstMatchPattern],
        captured: HashMap<VariableId, VorpalValue>,
        args: Vec<VorpalValue>,
        definition: &'v AstExpression,
    ) -> VResult<VorpalControlState<'v>> {
        if let Some(VorpalControl::CallBody) = thread.control.last() {
            // Tail call optimization!
            let top = thread.variables.last_mut().unwrap();
            top.clear();
            top.extend(captured);
        } else {
            thread.control.push(VorpalControl::CallBody);
            thread.variables.push(captured);
        }

        for (pattern, arg) in ZipExact::zip_exact(params, args, "args")
            .expect("ICE: parameter list length does not match args")
        {
            if !self.match_pattern(heap, thread, pattern, &arg)? {
                unreachable!("ICE: this pattern should be infallible: `{:#?}`", pattern);
            }
        }

        Ok(VorpalControlState::Expression(definition))
    }

    fn do_instruction<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        span: Span,
        instruction: &str,
        mut arguments: Vec<VorpalInstructionArgument<'v>>,
        output: &InstructionOutput,
    ) -> VResult<VorpalControlState<'v>> {
        // TODO: X: We should really be asserting these output types are sane...

        Ok(match arguments.len() {
            n if n >= 1 && instruction == "ch_call" => {
                let mut call_args = vec![];
                for a in arguments {
                    match a {
                        VorpalInstructionArgument::Value(a) => call_args.push(a),
                        _ => return vorpal_panic_at!(span, "Can only call values"),
                    }
                }

                self.do_value_call(heap, thread, span, call_args)?
            },
            n if n >= 2 && instruction == "ch_dynamic_dispatch" => {
                let fn_name =
                    if let VorpalInstructionArgument::Value(VorpalValue::String(fn_name)) =
                        arguments.remove(0)
                    {
                        fn_name
                    } else {
                        return vorpal_panic_at!(
                            span,
                            "can only dynamically dispatch if the first arg is a string fn-name"
                        );
                    };

                let mut call_args = vec![];
                for a in arguments {
                    match a {
                        VorpalInstructionArgument::Value(a) => call_args.push(a),
                        _ => unreachable!("ICE: Can only call values"),
                    }
                }

                self.do_dynamic_call(heap, thread, span, &fn_name, call_args)?
            },
            0 => match (instruction, output) {
                ("ch_thread_yield", InstructionOutput::Type(AstType::Tuple { types })) if types.is_empty() => {
                    if heap.num_ready_threads() > 0 {
                        // Park the current state, which is just the return value of this
                        // instruction == `()`.
                        thread
                            .control
                            .push(VorpalControl::ParkedState(VorpalControlState::Value(
                                VorpalValue::unit(),
                            )));

                        // Thread swapperoo.
                        let new_thread = heap.pop_thread()?;
                        debug!(
                            "Switching from thread {} -> {}",
                            thread.thread_id, new_thread.thread_id
                        );
                        let old_thread = std::mem::replace(thread, new_thread);
                        heap.push_thread(old_thread, false);

                        // Reset the timer, and return new thread initial state
                        thread.start = Instant::now();
                        VorpalControlState::Initial
                    } else {
                        // Reset the timer, and return new thread initial state
                        thread.start = Instant::now();
                        VorpalControlState::Value(VorpalValue::unit())
                    }
                },
                ("ch_thread_coalesce", InstructionOutput::Type(AstType::Tuple { types })) if types.is_empty() => {
                    if heap.num_all_threads(true) > 1 {
                        // Park the current state, which is just the return value of this
                        // instruction == `()`.
                        thread
                            .control
                            .push(VorpalControl::ParkedState(VorpalControlState::Value(
                                VorpalValue::unit(),
                            )));

                        let all_thread_ids = heap.get_other_thread_ids();

                        let new_thread = heap.pop_thread()?;
                        debug!(
                            "Switching from thread {} -> {} (coalescing thread {})",
                            thread.thread_id, new_thread.thread_id, thread.thread_id
                        );
                        let old_thread = std::mem::replace(thread, new_thread);
                        heap.block_thread(old_thread, &all_thread_ids, thread)?;

                        // Reset the timer, and return new thread initial state
                        thread.start = Instant::now();
                        VorpalControlState::Initial
                    } else {
                        VorpalControlState::Value(VorpalValue::unit())
                    }
                },
                ("ch_thread_current", InstructionOutput::Type(AstType::Object(_name, generics))) 
                    if /* TODO: S: heap.symbolizer.name_matches(name, "std::threading", "Thread") && */  generics.is_empty()
                    => VorpalControlState::Value(thread.thread_object.clone()),
                ("ch_thread_count", InstructionOutput::Type(AstType::Int)) =>
                    VorpalControlState::Value(VorpalValue::Int(heap.num_all_threads(true) as i64)),
                (_, _) =>
                    return vorpal_panic_at!(span, "Unknown nullary instruction `{}`", instruction,),
            },
            1 => {
                let v0 = arguments.into_iter().next().unwrap();
                match (instruction, v0, output) {
                    (
                        "ch_thread_block",
                        VorpalInstructionArgument::Value(VorpalValue::Int(other_id)),
                        InstructionOutput::Type(AstType::Tuple { types }),
                    ) if types.is_empty() => {
                        let other_id = ThreadId(other_id as usize);

                        if heap.is_thread_live(other_id) {
                            // Park the current state, which is just the return value of this
                            // instruction == `()`.
                            thread.control.push(VorpalControl::ParkedState(
                                VorpalControlState::Value(VorpalValue::unit()),
                            ));

                            let new_thread = heap.pop_thread()?;
                            debug!(
                                "Switching from thread {} -> {} (blocking thread {} on {})",
                                thread.thread_id, new_thread.thread_id, thread.thread_id, other_id
                            );
                            let old_thread = std::mem::replace(thread, new_thread);
                            heap.block_thread(old_thread, &[other_id], thread)?;

                            // Reset the timer, and return new thread initial state
                            thread.start = Instant::now();
                            VorpalControlState::Initial
                        } else {
                            VorpalControlState::Value(VorpalValue::unit())
                        }
                    },
                    (
                        "ch_awaitable_poll",
                        VorpalInstructionArgument::Value(VorpalValue::Awaitable(id)),
                        _, // TODO: S:
                    ) => self.do_await(heap, thread, span, id)?,
                    (instruction, v0, _) =>
                        return vorpal_panic_at!(
                            span,
                            "Unknown unary instruction `{}` called with args: `{:?}`",
                            instruction,
                            (v0,)
                        ),
                }
            },
            2 => {
                let (v0, v1) = arguments.into_iter().tuples().next().unwrap();
                match (instruction, v0, v1, output) {
                    (
                        "ch_dynamic_unbox",
                        VorpalInstructionArgument::Value(v0 @ VorpalValue::DynamicBox(_)),
                        VorpalInstructionArgument::Type(v1),
                        _, // TODO: S: type is Option of v1
                    ) => {
                        let value = self.try_dynamic_unbox(heap, thread, v0, v1)?;
                        VorpalControlState::Value(value)
                    },
                    (instruction, v0, v1, _) =>
                        return vorpal_panic_at!(
                            span,
                            "Unknown binary instruction `{}` called with args: `{:?}`",
                            instruction,
                            (v0, v1)
                        ),
                }
            },
            3 => {
                let (v0, v1, v2) = arguments.into_iter().tuples().next().unwrap();
                match (instruction, v0, v1, v2, output) {
                    (
                        "ch_dynamic_box",
                        VorpalInstructionArgument::Value(v0),
                        VorpalInstructionArgument::Type(v1),
                        VorpalInstructionArgument::Type(v2),
                        InstructionOutput::Type(v3),
                    ) if v2 == v3 => {
                        let dynamic = self.make_dynamic_box(heap, thread, v0, v1, v2)?;
                        VorpalControlState::Value(dynamic)
                    },
                    (
                        "ch_dynamic_transmute",
                        VorpalInstructionArgument::Value(v0 @ VorpalValue::DynamicBox(_)),
                        VorpalInstructionArgument::Type(v1),
                        VorpalInstructionArgument::Type(v2),
                        InstructionOutput::Type(v3),
                    ) if v2 == v3 => {
                        let dynamic = self.transmute_dynamic_box(heap, thread, v0, v1, v2)?;
                        VorpalControlState::Value(dynamic)
                    },
                    (
                        "ch_thread_spawn",
                        VorpalInstructionArgument::Value(trampoline),
                        VorpalInstructionArgument::Value(callable),
                        VorpalInstructionArgument::Type(exit_type),
                        _, // TODO: S: type is std::threading::Thread
                    ) => {
                        // Make a new thread
                        let mut new_thread = heap.new_thread(self.type_ids[exit_type]);
                        // Set up the call trampoline(callable)
                        let state = self.do_value_call(heap, &mut new_thread, span, vec![
                            trampoline, callable,
                        ])?;
                        new_thread.control.push(VorpalControl::ParkedState(state));
                        // Then push it so we can switch to it later...
                        let thread_object = new_thread.thread_object.clone();
                        debug!(
                            "Spawned thread {} (from thread {})",
                            new_thread.thread_id, thread.thread_id
                        );
                        heap.push_thread(new_thread, false);

                        VorpalControlState::Value(thread_object)
                    },
                    (instruction, v0, v1, v2, _) => unreachable!(
                        "Unknown ternary instruction `{}` called with args: `{:?}`",
                        instruction,
                        (v0, v1, v2)
                    ),
                }
            },
            _ => unreachable!(
                "Unknown N-ary instruction `{}` called with args: `{:?}`",
                instruction, arguments
            ),
        })
    }

    fn match_pattern<'v>(
        &self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        pattern: &'v AstMatchPattern,
        value: &VorpalValue,
    ) -> VResult<bool> {
        Ok(match (&pattern.data, value) {
            (AstMatchPatternData::Underscore, _) => true,
            (AstMatchPatternData::Literal(AstLiteral::True), VorpalValue::Int(1)) => true,
            (AstMatchPatternData::Literal(AstLiteral::False), VorpalValue::Int(0)) => true,
            // TODO: I: Probably want to pre-parse these at compile time.
            (AstMatchPatternData::Literal(AstLiteral::Int(int1)), VorpalValue::Int(int2)) =>
                int1.parse::<i64>().unwrap() == *int2,
            (
                AstMatchPatternData::Literal(AstLiteral::Float(float1)),
                VorpalValue::Float(float2),
            ) => float1.parse::<f64>().unwrap() == *float2,
            (AstMatchPatternData::Literal(AstLiteral::Char(chr1)), VorpalValue::Int(chr2)) =>
                (*chr1 as i64) == *chr2,
            (AstMatchPatternData::Literal(AstLiteral::String(str1)), VorpalValue::String(str2)) =>
                str1 == str2,
            (AstMatchPatternData::Identifier(var1), value) => {
                thread
                    .variables
                    .last_mut()
                    .unwrap()
                    .insert(var1.id, value.clone());
                true
            },
            (
                AstMatchPatternData::Tuple(pattern_children),
                VorpalValue::ValueCollection { values },
            ) => {
                let mut matches = true;

                for (pat_child, val_child) in
                    ZipExact::zip_exact(pattern_children, values, "tuple members")
                        .expect("ICE: Tuples should be same arity")
                {
                    if !self.match_pattern(heap, thread, pat_child, val_child)? {
                        matches = false;
                        break;
                    }
                }

                matches
            },
            (
                AstMatchPatternData::PositionalEnum {
                    variant: pattern_variant,
                    children: pattern_children,
                    ..
                },
                VorpalValue::EnumVariant {
                    variant: value_variant,
                    values,
                },
            ) if pattern_variant == value_variant => {
                let mut matches = true;

                for (pat_child, tup_child) in
                    ZipExact::zip_exact(pattern_children, values, "enum members")
                        .expect("ICE: Enum variants should be same arity")
                {
                    if !self.match_pattern(heap, thread, pat_child, tup_child)? {
                        matches = false;
                        break;
                    }
                }

                matches
            },
            _ => false,
        })
    }

    fn do_await<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        thread: &mut VorpalThread<'v>,
        span: Span,
        id: VorpalAwaitablePointer,
    ) -> VResult<VorpalControlState<'v>> {
        let awaitable = heap.get_awaitable(id);

        let new_awaitable;
        let new_state;

        match std::mem::replace(awaitable, VorpalAwaitable::Evaluating) {
            VorpalAwaitable::Incomplete {
                stack_rev,
                variables,
                state,
            } => {
                thread.control.extend(stack_rev.into_iter().rev());
                thread.variables.push(variables);

                new_state = state;
                new_awaitable = VorpalAwaitable::Evaluating;
            },
            VorpalAwaitable::Evaluating => {
                return vorpal_panic_at!(
                    span,
                    "Infinite recursion in `awaitable`. Are you awaiting an awaitable already on \
                     the call stack?"
                );
            },
            VorpalAwaitable::Complete(value) => {
                new_state = VorpalControlState::Value(VorpalValue::ValueCollection {
                    values: vec![
                        VorpalValue::EnumVariant {
                            variant: "Complete".to_string(),
                            values: vec![value.clone()],
                        },
                        VorpalValue::Awaitable(id),
                    ],
                });
                new_awaitable = VorpalAwaitable::Complete(value);
            },
        }

        *awaitable = new_awaitable;
        Ok(new_state)
    }

    fn make_plain_dynamic_box<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        _: &mut VorpalThread<'v>,
        obj: VorpalValue,
        obj_type_id: usize,
    ) -> VResult<VorpalValue> {
        // TODO: I: make a plain dynamic box???
        Ok(heap.allocate_dynamic_box(obj_type_id, obj, hashmap! {}))
    }

    fn make_dynamic_box<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        _: &mut VorpalThread<'v>,
        obj: VorpalValue,
        obj_ty: &AstType,
        dyn_ty: &AstType,
    ) -> VResult<VorpalValue> {
        if let AstType::DynamicType { trait_tys } = dyn_ty {
            let mut table = hashmap! {};

            for AstTraitTypeWithAssocs { trt, .. } in trait_tys {
                let key = (obj_ty.clone(), trt.clone());

                if self.program_object_tables.contains_key(&key) {
                    for (decorated_name, signature) in &self.program_object_tables[&key] {
                        table.insert(decorated_name.clone(), signature.clone());
                    }
                }
            }

            Ok(heap.allocate_dynamic_box(self.type_ids[obj_ty], obj, table))
        } else {
            unreachable!("ICE: Can only box if given a dynamic type")
        }
    }

    fn transmute_dynamic_box<'v>(
        &'v self,
        _: &mut VorpalHeap<'v>,
        _: &mut VorpalThread<'v>,
        obj: VorpalValue,
        _: &'v AstType,
        _: &'v AstType,
    ) -> VResult<VorpalValue> {
        if let VorpalValue::DynamicBox(id) = obj {
            Ok(VorpalValue::DynamicBox(id))
        } else {
            unreachable!("ICE: can only transmute a DynamicBox, got `{:?}`", obj)
        }
    }

    fn try_dynamic_unbox<'v>(
        &'v self,
        heap: &mut VorpalHeap<'v>,
        _: &mut VorpalThread<'v>,
        object: VorpalValue,
        target_ty: &'v AstType,
    ) -> VResult<VorpalValue> {
        if let VorpalValue::DynamicBox(id) = object {
            let VorpalDynBox {
                type_id, object, ..
            } = heap.get_dyn_box(id);

            if *type_id == self.type_ids[&target_ty] {
                Ok(VorpalValue::EnumVariant {
                    variant: "Some".to_string(),
                    values: vec![object.clone()],
                })
            } else {
                Ok(VorpalValue::EnumVariant {
                    variant: "None".to_string(),
                    values: vec![],
                })
            }
        } else {
            unreachable!("ICE: can only unbox a DynamicBox, got `{:?}`", object)
        }
    }

    fn find_instruction_arg<'v>(
        &self,
        uneval_rev: &mut Vec<&'v InstructionArgument>,
        args: &mut Vec<VorpalInstructionArgument<'v>>,
    ) -> Option<&'v AstExpression> {
        loop {
            match uneval_rev.pop() {
                Some(InstructionArgument::Type(t)) => args.push(VorpalInstructionArgument::Type(t)),
                Some(InstructionArgument::Anonymous(a)) =>
                    args.push(VorpalInstructionArgument::Anonymous(a)),
                Some(InstructionArgument::Expression(e)) => return Some(e),
                None => return None,
            }
        }
    }
}

pub fn is_dyn_dispatchable(fun: &AstObjectFunction) -> bool {
    if !fun.has_self {
        return false;
    }

    if !fun.restrictions.is_empty() {
        return false;
    }

    if !fun.generics.is_empty() {
        return false;
    }

    true
}
