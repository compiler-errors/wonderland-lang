	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_f4main                 ## -- Begin function f4main
	.p2align	4, 0x90
_f4main:                                ## @f4main
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, ___gcc_personality_v0
	.cfi_lsda 16, Lexception0
## %bb.0:                               ## %pre
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -16
	movq	_llvm_gc_root_chain@GOTPCREL(%rip), %rbx
	movq	(%rbx), %rax
	leaq	___gc_f4main(%rip), %rcx
	movq	%rcx, 16(%rsp)
	movq	$0, 24(%rsp)
	movq	%rax, 8(%rsp)
	leaq	8(%rsp), %rax
	movq	%rax, (%rbx)
Ltmp0:
	movl	$8, %edi
	movl	$3, %esi
	callq	_alloc_array
Ltmp1:
## %bb.1:                               ## %temp_2.noexc
	movq	8(%rax), %rcx
	movq	$1, (%rcx)
	movq	8(%rax), %rcx
	movq	$2, 8(%rcx)
	movq	8(%rax), %rcx
	movq	$3, 16(%rcx)
	movq	%rax, 24(%rsp)
Ltmp2:
	movq	%rax, %rdi
	callq	_f3foo
Ltmp3:
## %bb.2:                               ## %temp_16.noexc
	movq	$0, 24(%rsp)
	movq	8(%rsp), %rcx
	movq	%rcx, (%rbx)
	addq	$32, %rsp
	popq	%rbx
	retq
LBB0_3:                                 ## %gc_cleanup
Ltmp4:
	movq	8(%rsp), %rcx
	movq	%rcx, (%rbx)
	movq	%rax, %rdi
	callq	__Unwind_Resume
	ud2
Lfunc_end0:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
Lexception0:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	255                     ## @TType Encoding = omit
	.byte	1                       ## Call site Encoding = uleb128
	.uleb128 Lcst_end0-Lcst_begin0
Lcst_begin0:
	.uleb128 Ltmp0-Lfunc_begin0     ## >> Call Site 1 <<
	.uleb128 Ltmp3-Ltmp0            ##   Call between Ltmp0 and Ltmp3
	.uleb128 Ltmp4-Lfunc_begin0     ##     jumps to Ltmp4
	.byte	0                       ##   On action: cleanup
	.uleb128 Ltmp3-Lfunc_begin0     ## >> Call Site 2 <<
	.uleb128 Lfunc_end0-Ltmp3       ##   Call between Ltmp3 and Lfunc_end0
	.byte	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lcst_end0:
	.p2align	2
                                        ## -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_f3foo                  ## -- Begin function f3foo
	.p2align	4, 0x90
_f3foo:                                 ## @f3foo
	.cfi_startproc
## %bb.0:                               ## %pre
	movq	_llvm_gc_root_chain@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rdx
	leaq	___gc_f3foo(%rip), %rax
	movq	%rax, -24(%rsp)
	movq	%rdi, -16(%rsp)
	movq	%rdx, -32(%rsp)
	leaq	-32(%rsp), %rax
	movq	%rax, (%rcx)
	movq	%rdi, -8(%rsp)
	movq	8(%rdi), %rax
	movq	$0, -8(%rsp)
	movq	8(%rax), %rax
	movq	%rdx, (%rcx)
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__DATA,__data
	.globl	_llvm_gc_root_chain     ## @llvm_gc_root_chain
	.weak_definition	_llvm_gc_root_chain
	.p2align	3
_llvm_gc_root_chain:
	.quad	0

	.section	__TEXT,__const
	.p2align	3               ## @__gc_f4main
___gc_f4main:
	.long	1                       ## 0x1
	.long	0                       ## 0x0

	.p2align	3               ## @__gc_f3foo
___gc_f3foo:
	.long	2                       ## 0x2
	.long	0                       ## 0x0


.subsections_via_symbols
