use super::thread::{VError, VResult, VorpalThread};
use crate::{
    ana::represent::AnalyzedProgram,
    ast::{AstType, ModuleRef},
    util::PResult,
    vs::{heap::VorpalHeap, value::VorpalValue},
};
use itertools::Itertools;
use std::collections::HashMap;

const TYCK_COMPLAINT: &'static str = "ICE: type checker should not have allowed this!";

pub type VorpalExternFn = for<'v> fn(
    &mut VorpalHeap<'v>,
    &mut VorpalThread<'v>,
    &[AstType],
    Vec<VorpalValue>,
) -> VResult<VorpalValue>;

pub fn get_extern_fns<'v>(
    program: &AnalyzedProgram,
) -> PResult<HashMap<ModuleRef, VorpalExternFn>> {
    let mut x: HashMap<ModuleRef, VorpalExternFn> = HashMap::new();

    x.insert(program.construct_fn_ref("std::print")?, print);
    x.insert(program.construct_fn_ref("std::breakpoint")?, breakpoint);
    x.insert(program.construct_fn_ref("std::unreachable")?, unreachable);
    x.insert(program.construct_fn_ref("std::exit")?, exit);
    x.insert(program.construct_fn_ref("std::gc")?, gc);
    x.insert(program.construct_fn_ref("std::type_string")?, type_string);

    x.insert(
        program.construct_fn_ref("std::operators::predicate::internal_eq")?,
        binop_int::<eq>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::predicate::internal_gt")?,
        binop_int::<gt>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::predicate::internal_feq")?,
        binop_float::<feq>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::predicate::internal_fgt")?,
        binop_float::<fgt>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::predicate::internal_string_eq")?,
        string_eq,
    );

    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_add")?,
        binop_int::<add>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_sub")?,
        binop_int::<sub>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_mul")?,
        binop_int::<mul>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_div")?,
        binop_int::<div>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_rem")?,
        binop_int::<rem>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_xor")?,
        binop_int::<xor>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_csub")?,
        binop_int::<sub>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_lshr")?,
        binop_int::<lshr>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_neg")?,
        unop_int::<neg>,
    );

    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_fadd")?,
        binop_float::<fadd>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_fsub")?,
        binop_float::<fsub>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_fmul")?,
        binop_float::<fmul>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_fdiv")?,
        binop_float::<fdiv>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_fneg")?,
        unop_float::<fneg>,
    );

    x.insert(
        program.construct_fn_ref("std::operators::arithmetic::internal_string_add")?,
        string_add,
    );

    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_itos")?,
        unop_int::<itos>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_ftos")?,
        unop_float::<ftos>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_ctos")?,
        unop_int::<ctos>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_ctoi")?,
        unop_int::<ctoi>,
    );
    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_itof")?,
        unop_int::<itof>,
    );

    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_alloc_empty_array")?,
        alloc_empty_array,
    );
    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_array_len")?,
        array_len,
    );
    x.insert(
        program.construct_fn_ref("std::operators::lang::internal_string_len")?,
        string_len,
    );

    x.insert(
        program.construct_fn_ref("std::operators::access::internal_array_deref")?,
        array_deref,
    );
    x.insert(
        program.construct_fn_ref("std::operators::access::internal_array_store")?,
        array_store,
    );
    x.insert(
        program.construct_fn_ref("std::operators::access::internal_array_slice")?,
        array_slice,
    );
    x.insert(
        program.construct_fn_ref("std::operators::access::internal_string_deref")?,
        string_deref,
    );

    Ok(x)
}

fn print<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 1, TYCK_COMPLAINT);
    let a0 = args.into_iter().next().expect(TYCK_COMPLAINT);

    if let VorpalValue::String(a0) = a0 {
        print!("{}", a0);
    } else {
        unreachable!(TYCK_COMPLAINT)
    }

    Ok(VorpalValue::unit())
}

fn gc<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    _: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    todo!("TODO: F: GC")
}

fn unreachable<'v>(
    h: &mut VorpalHeap<'v>,
    t: &mut VorpalThread<'v>,
    g: &[AstType],
    _: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    println!("Reached an unreachable statement");
    exit(h, t, g, vec![VorpalValue::Int(255)])
}

fn exit<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 1, TYCK_COMPLAINT);
    let a0 = args.into_iter().next().expect(TYCK_COMPLAINT);

    if let VorpalValue::Int(a0) = a0 {
        Err(VError::Exit(a0))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn type_string<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    generics: &[AstType],
    _: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(generics.len() == 1, TYCK_COMPLAINT);
    Ok(VorpalValue::String(format!("{}", generics[0])))
}

#[inline(never)]
fn breakpoint<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    _: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    Ok(VorpalValue::unit())
}

fn binop_int<'v, const X: fn(i64, i64) -> VorpalValue>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 2, TYCK_COMPLAINT);
    let (v0, v1) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (VorpalValue::Int(v0), VorpalValue::Int(v1)) = (v0, v1) {
        Ok(X(v0, v1))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn unop_int<'v, const X: fn(i64) -> VorpalValue>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 1, TYCK_COMPLAINT);
    let v0 = args.into_iter().next().expect(TYCK_COMPLAINT);

    if let VorpalValue::Int(v0) = v0 {
        Ok(X(v0))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn unop_float<'v, const X: fn(f64) -> VorpalValue>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 1, TYCK_COMPLAINT);
    let v0 = args.into_iter().next().expect(TYCK_COMPLAINT);

    if let VorpalValue::Float(v0) = v0 {
        Ok(X(v0))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn binop_float<'v, const X: fn(f64, f64) -> VorpalValue>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 2, TYCK_COMPLAINT);
    let (v0, v1) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (VorpalValue::Float(v0), VorpalValue::Float(v1)) = (v0, v1) {
        Ok(X(v0, v1))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn add(a: i64, b: i64) -> VorpalValue {
    VorpalValue::Int(a.wrapping_add(b))
}

fn sub(a: i64, b: i64) -> VorpalValue {
    VorpalValue::Int(a.wrapping_sub(b))
}

fn mul(a: i64, b: i64) -> VorpalValue {
    VorpalValue::Int(a.wrapping_mul(b))
}

fn div(a: i64, b: i64) -> VorpalValue {
    VorpalValue::Int(a.wrapping_div(b))
}

fn rem(a: i64, b: i64) -> VorpalValue {
    VorpalValue::Int(a.wrapping_rem(b))
}

fn xor(a: i64, b: i64) -> VorpalValue {
    VorpalValue::Int(a ^ b)
}

fn gt(a: i64, b: i64) -> VorpalValue {
    VorpalValue::boolean(a > b)
}

fn eq(a: i64, b: i64) -> VorpalValue {
    VorpalValue::boolean(a == b)
}

fn lshr(a: i64, b: i64) -> VorpalValue {
    VorpalValue::Int(((a as u64) << (b as u64)) as i64)
}

fn neg(a: i64) -> VorpalValue {
    VorpalValue::Int(-a)
}

fn fgt(a: f64, b: f64) -> VorpalValue {
    VorpalValue::boolean(a > b)
}

fn feq(a: f64, b: f64) -> VorpalValue {
    VorpalValue::boolean(a == b)
}

fn fadd(a: f64, b: f64) -> VorpalValue {
    VorpalValue::Float(a + b)
}

fn fsub(a: f64, b: f64) -> VorpalValue {
    VorpalValue::Float(a - b)
}

fn fmul(a: f64, b: f64) -> VorpalValue {
    VorpalValue::Float(a * b)
}

fn fdiv(a: f64, b: f64) -> VorpalValue {
    VorpalValue::Float(a / b)
}

fn fneg(a: f64) -> VorpalValue {
    VorpalValue::Float(-a)
}

fn itos(a: i64) -> VorpalValue {
    VorpalValue::String(format!("{}", a))
}

fn ftos(a: f64) -> VorpalValue {
    VorpalValue::String(format!("{}", a))
}

fn ctos(a: i64) -> VorpalValue {
    VorpalValue::String(format!("{}", a as u8 as char))
}

fn ctoi(a: i64) -> VorpalValue {
    VorpalValue::Int(a)
}

fn itof(a: i64) -> VorpalValue {
    VorpalValue::Float(a as f64)
}

fn string_eq<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 2, TYCK_COMPLAINT);
    let (v0, v1) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (VorpalValue::String(v0), VorpalValue::String(v1)) = (v0, v1) {
        Ok(VorpalValue::boolean(v0 == v1))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn string_add<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 2, TYCK_COMPLAINT);
    let (v0, v1) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (VorpalValue::String(mut v0), VorpalValue::String(v1)) = (v0, v1) {
        v0.push_str(&v1);
        Ok(VorpalValue::String(v0))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn array_deref<'v>(
    heap: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 2, TYCK_COMPLAINT);
    let (v0, v1) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (v0 @ VorpalValue::HeapCollection { .. }, VorpalValue::Int(idx)) = (v0, v1) {
        if idx < 0 {
            return vorpal_panic!("Array index ({}) cannot be less than 0.", idx,);
        }

        Ok(heap.get_object_idx(v0, idx as usize)?.clone())
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn array_store<'v>(
    heap: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 3, TYCK_COMPLAINT);
    let (v0, v1, v2) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (v0 @ VorpalValue::HeapCollection { .. }, VorpalValue::Int(idx)) = (v0, v1) {
        if idx < 0 {
            return vorpal_panic!("Array index ({}) cannot be less than 0.", idx,);
        }

        *heap.get_object_idx(v0, idx as usize)? = v2.clone();
        Ok(v2)
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn array_slice<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 3, TYCK_COMPLAINT);
    let (v0, v1, v2) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (v0 @ VorpalValue::HeapCollection { .. }, VorpalValue::Int(v1), VorpalValue::Int(v2)) =
        (v0, v1, v2)
    {
        if v1 < 0 {
            return vorpal_panic!("Start of array slice ({}) cannot be less than 0.", v1,);
        }

        if v2 < 0 {
            return vorpal_panic!("End of array slice ({}) cannot be less than 0.", v2,);
        }

        Ok(v0.get_object_slice(v1 as usize, v2 as usize)?)
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn string_deref<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 2, TYCK_COMPLAINT);
    let (v0, v1) = args.into_iter().tuples().next().expect(TYCK_COMPLAINT);

    if let (VorpalValue::String(v0), VorpalValue::Int(idx)) = (v0, v1) {
        if idx < 0 || v0.len() <= idx as usize {
            return vorpal_panic!("String deref out of bounds, len={}, idx={}", v0.len(), idx);
        }

        Ok(VorpalValue::Int(
            v0.chars().nth(idx as usize).unwrap() as u8 as i64,
        ))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn alloc_empty_array<'v>(
    heap: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 1, TYCK_COMPLAINT);
    let v0 = args.into_iter().next().expect(TYCK_COMPLAINT);

    if let VorpalValue::Int(v0) = v0 {
        Ok(heap.allocate_heap_object(vec![VorpalValue::Undefined; v0 as usize]))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn array_len<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 1, TYCK_COMPLAINT);
    let v0 = args.into_iter().next().expect(TYCK_COMPLAINT);

    if let VorpalValue::HeapCollection { offset, limit, .. } = v0 {
        Ok(VorpalValue::Int((limit as i64) - (offset as i64)))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}

fn string_len<'v>(
    _: &mut VorpalHeap<'v>,
    _: &mut VorpalThread<'v>,
    _: &[AstType],
    args: Vec<VorpalValue>,
) -> VResult<VorpalValue> {
    assert!(args.len() == 1, TYCK_COMPLAINT);
    let v0 = args.into_iter().next().expect(TYCK_COMPLAINT);

    if let VorpalValue::String(v0) = v0 {
        Ok(VorpalValue::Int(v0.len() as i64))
    } else {
        unreachable!(TYCK_COMPLAINT)
    }
}
