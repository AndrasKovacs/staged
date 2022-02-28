	.text
	.intel_syntax noprefix
	.file	"gcnotes.c"
	.globl	map                     # -- Begin function map
	.p2align	4, 0x90
	.type	map,@function
map:                                    # @map
# %bb.0:
	mov	rax, qword ptr [r14]
	mov	rcx, rax
	shr	rcx, 58
	jne	.LBB0_2
# %bb.1:
	mov	qword ptr [r13], 0
	mov	qword ptr [r13 + 8], rbp
	mov	qword ptr [r13 + 16], r12
	mov	rax, r13
	ret
.LBB0_2:
	sub	rsp, 40
	movabs	r14, 288230376151711744
	shl	rax, 19
	sar	rax, 16
	mov	r15, qword ptr [rax]
	mov	rax, qword ptr [rax + 8]
	mov	qword ptr [rsp + 8], rax
	lea	rdi, [rsp + 16]
	lea	r8, [rsp + 8]
	mov	rsi, rbp
	mov	rdx, r12
	mov	rcx, rbx
	call	map
	mov	rdi, qword ptr [rsp + 24]
	mov	rsi, qword ptr [rsp + 32]
	lea	rcx, [rdi + 16]
	cmp	rcx, rsi
	jae	.LBB0_3
.LBB0_4:
	mov	qword ptr [rax], r15
	mov	rcx, qword ptr [rsp + 16]
	mov	qword ptr [rax + 8], rcx
	mov	rbp, rax
	shr	rbp, 3
	or	rbp, r14
	lea	rdi, [rax + 16]
	cmp	rdi, rdx
	jae	.LBB0_5
.LBB0_6:
	mov	qword ptr [rcx], 100
	mov	qword ptr [rcx + 8], rbp
	mov	rax, rcx
	shr	rax, 3
	or	rax, r14
	mov	qword ptr [r13], rax
	mov	qword ptr [r13 + 8], rcx
	mov	qword ptr [r13 + 16], rsi
	add	rsp, 40
	mov	rax, r13
	ret
.LBB0_3:
	mov	rdx, rbx
	call	perform_gc
	jmp	.LBB0_4
.LBB0_5:
	mov	rdi, rax
	mov	rsi, rdx
	mov	rdx, rbx
	call	perform_gc
	mov	rcx, rax
	mov	rsi, rdx
	jmp	.LBB0_6
.Lfunc_end0:
	.size	map, .Lfunc_end0-map
                                        # -- End function

	.ident	"clang version 7.1.0 (tags/RELEASE_710/final)"
	.section	".note.GNU-stack","",@progbits
