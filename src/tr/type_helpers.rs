use crate::util::PResult;
use either::Either;
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValueEnum, CallSiteValue, IntValue};
use inkwell::AddressSpace;

pub fn fun_type(t: BasicTypeEnum, p: &[BasicTypeEnum]) -> FunctionType {
    match t {
        BasicTypeEnum::StructType(t) => t.fn_type(p, false),
        BasicTypeEnum::ArrayType(t) => t.fn_type(p, false),
        BasicTypeEnum::IntType(t) => t.fn_type(p, false),
        BasicTypeEnum::FloatType(t) => t.fn_type(p, false),
        BasicTypeEnum::PointerType(t) => t.fn_type(p, false),
        BasicTypeEnum::VectorType(t) => t.fn_type(p, false),
    }
}

pub fn ptr_type(t: BasicTypeEnum, a: AddressSpace) -> BasicTypeEnum {
    match t {
        BasicTypeEnum::StructType(t) => t.ptr_type(a),
        BasicTypeEnum::ArrayType(t) => t.ptr_type(a),
        BasicTypeEnum::IntType(t) => t.ptr_type(a),
        BasicTypeEnum::FloatType(t) => t.ptr_type(a),
        BasicTypeEnum::PointerType(t) => t.ptr_type(a),
        BasicTypeEnum::VectorType(t) => t.ptr_type(a),
    }
    .into()
}

pub fn type_zero(t: BasicTypeEnum) -> PResult<BasicValueEnum> {
    let ty: BasicValueEnum = match t {
        BasicTypeEnum::StructType(t) => t.const_zero().into(),
        BasicTypeEnum::ArrayType(t) => t.const_zero().into(),
        BasicTypeEnum::IntType(t) => t.const_zero().into(),
        BasicTypeEnum::FloatType(t) => t.const_zero().into(),
        BasicTypeEnum::PointerType(t) => t.const_zero().into(),
        BasicTypeEnum::VectorType(t) => t.const_zero().into(),
    };

    Ok(ty)
}

pub fn type_size(t: BasicTypeEnum) -> PResult<IntValue> {
    let ty = match t {
        BasicTypeEnum::StructType(t) => t.size_of().unwrap(),
        BasicTypeEnum::ArrayType(t) => t.size_of().unwrap(),
        BasicTypeEnum::IntType(t) => t.size_of(),
        BasicTypeEnum::FloatType(t) => t.size_of(),
        BasicTypeEnum::PointerType(t) => t.size_of(),
        BasicTypeEnum::VectorType(t) => t.size_of().unwrap(),
    };

    Ok(ty)
}

pub fn unwrap_callsite(callsite: CallSiteValue) -> BasicValueEnum {
    match callsite.try_as_basic_value() {
        Either::Left(x) => x,
        Either::Right(_) => unreachable!(),
    }
}
