	.text
	.intel_syntax noprefix
	.file	"llvmnotes.c"
	.globl	bar                     # -- Begin function bar
	.p2align	4, 0x90
	.type	bar,@function
bar:                                    # @bar
# %bb.0:
	imul	r13, rbp
	mov	rax, r13
	ret
.Lfunc_end0:
	.size	bar, .Lfunc_end0-bar
                                        # -- End function
	.globl	foo                     # -- Begin function foo
	.p2align	4, 0x90
	.type	foo,@function
foo:                                    # @foo
# %bb.0:
	imul	r13, r12
	imul	r13, rbx
	imul	r13, r14
	imul	r13, rsi
	imul	r13, rdi
	imul	r13, r8
	imul	r13, r9
	imul	r13, r15
	imul	r13, rdx
	imul	r13, rcx
	imul	r13, rbp
	imul	r13, qword ptr [rsp + 8]
	imul	r13, qword ptr [rsp + 16]
	imul	r13, qword ptr [rsp + 24]
	imul	r13, qword ptr [rsp + 32]
	mov	rax, r13
	ret
.Lfunc_end1:
	.size	foo, .Lfunc_end1-foo
                                        # -- End function
	.globl	fact_go                 # -- Begin function fact_go
	.p2align	4, 0x90
	.type	fact_go,@function
fact_go:                                # @fact_go
# %bb.0:
	test	rdi, rdi
	je	.LBB2_2
	.p2align	4, 0x90
.LBB2_1:                                # =>This Inner Loop Header: Depth=1
	imul	rsi, rdi
	dec	rdi
	jne	.LBB2_1
.LBB2_2:
	mov	rax, rsi
	ret
.Lfunc_end2:
	.size	fact_go, .Lfunc_end2-fact_go
                                        # -- End function
	.globl	fuckghc                 # -- Begin function fuckghc
	.p2align	4, 0x90
	.type	fuckghc,@function
fuckghc:                                # @fuckghc
	.cfi_startproc
# %bb.0:
	imul	rdi, rsi
	mov	rax, rdi
	ret
.Lfunc_end3:
	.size	fuckghc, .Lfunc_end3-fuckghc
	.cfi_endproc
                                        # -- End function
	.globl	fuckghc2                # -- Begin function fuckghc2
	.p2align	4, 0x90
	.type	fuckghc2,@function
fuckghc2:                               # @fuckghc2
	.cfi_startproc
# %bb.0:
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset rbx, -16
	mov	rbx, rdi
	call	fuckghc
	imul	rax, rbx
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
.Lfunc_end4:
	.size	fuckghc2, .Lfunc_end4-fuckghc2
	.cfi_endproc
                                        # -- End function

	.ident	"clang version 7.1.0 (tags/RELEASE_710/final)"
	.section	".note.GNU-stack","",@progbits
