	.text
	.intel_syntax noprefix
	.file	"structret.c"
	.p2align	4, 0x90         # -- Begin function fun3
	.type	.Lfun3,@function
.Lfun3:                                 # @fun3
# %bb.0:
	mov	rax, r13
	mov	rdx, rbp
	mov	rcx, r12
	ret
.Lfunc_end0:
	.size	.Lfun3, .Lfunc_end0-.Lfun3
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function fun
	.type	.Lfun,@function
.Lfun:                                  # @fun
# %bb.0:
	mov	qword ptr [r13 + 24], r14
	mov	qword ptr [r13 + 16], rbx
	mov	qword ptr [r13 + 8], r12
	mov	qword ptr [r13], rbp
	mov	rax, r13
	ret
.Lfunc_end1:
	.size	.Lfun, .Lfunc_end1-.Lfun
                                        # -- End function

	.ident	"clang version 7.1.0 (tags/RELEASE_710/final)"
	.section	".note.GNU-stack","",@progbits
