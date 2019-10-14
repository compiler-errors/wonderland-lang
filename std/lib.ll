declare void @foo()

define i64 @f39std.operators.internal.internal_add_int(i64 %a, i64 %b) alwaysinline {
    %c = add i64 %a, %b
    ret i64 %c
}

define i64 @f39std.operators.internal.internal_sub_int(i64 %a, i64 %b) alwaysinline {
    %c = sub i64 %a, %b
    ret i64 %c
}

define i64 @f39std.operators.internal.internal_mul_int(i64 %a, i64 %b) alwaysinline {
    %c = mul i64 %a, %b
    ret i64 %c
}

define i64 @f39std.operators.internal.internal_div_int(i64 %a, i64 %b) alwaysinline {
    %c = sdiv i64 %a, %b
    ret i64 %c
}

define i64 @f39std.operators.internal.internal_mod_int(i64 %a, i64 %b) alwaysinline {
    %c = srem i64 %a, %b
    ret i64 %c
}

define i1 @f38std.operators.internal.internal_eq_int(i64 %a, i64 %b) alwaysinline {
    %c = icmp eq i64 %a, %b
    ret i1 %c
}

define i1 @f38std.operators.internal.internal_gt_int(i64 %a, i64 %b) alwaysinline {
    %c = icmp sgt i64 %a, %b
    ret i1 %c
}