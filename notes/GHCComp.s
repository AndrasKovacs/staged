	.text
	.file	"ghc_1.ll"
	.globl	GHCComp_bar_info$def    # -- Begin function GHCComp_bar_info$def
	.p2align	4, 0x90
	.type	GHCComp_bar_info$def,@object
	.quad	8589934604              # @"GHCComp_bar_info$def"
                                        # 0x20000000c
	.quad	0                       # 0x0
	.long	14                      # 0xe
	.long	0                       # 0x0
GHCComp_bar_info$def:
# %bb.0:                                # %c1fw
	imulq	%rsi, %r14
	movq	(%rbp), %rax
	movq	%r14, %rbx
	jmpq	*%rax                   # TAILCALL
.Lfunc_end0:
	.size	GHCComp_bar_info$def, .Lfunc_end0-GHCComp_bar_info$def
                                        # -- End function
	.globl	GHCComp_foo_info$def    # -- Begin function GHCComp_foo_info$def
	.p2align	4, 0x90
	.type	GHCComp_foo_info$def,@object
	.quad	8589934604              # @"GHCComp_foo_info$def"
                                        # 0x20000000c
	.quad	0                       # 0x0
	.long	14                      # 0xe
	.long	0                       # 0x0
GHCComp_foo_info$def:
# %bb.0:                                # %c1fL
	leaq	-16(%rbp), %rax
	cmpq	%r15, %rax
	jb	.LBB1_2
# %bb.1:                                # %c1fQ
	movq	$c1fJ_info$def, -16(%rbp)
	movq	%r14, -8(%rbp)
	imulq	%r14, %r14
	imulq	%rsi, %r14
	movq	(%rbp), %rax
	movq	%r14, %rbx
	jmpq	*%rax                   # TAILCALL
.LBB1_2:                                # %c1fP
	movq	-8(%r13), %rax
	movl	$GHCComp_foo_closure, %ebx
	jmpq	*%rax                   # TAILCALL
.Lfunc_end1:
	.size	GHCComp_foo_info$def, .Lfunc_end1-GHCComp_foo_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c1fJ_info$def
	.type	c1fJ_info$def,@object
	.quad	65                      # @"c1fJ_info$def"
                                        # 0x41
	.long	30                      # 0x1e
	.long	0                       # 0x0
c1fJ_info$def:
# %bb.0:                                # %c1fJ
	imulq	8(%rbp), %rbx
	movq	16(%rbp), %rax
	addq	$16, %rbp
	jmpq	*%rax                   # TAILCALL
.Lfunc_end2:
	.size	c1fJ_info$def, .Lfunc_end2-c1fJ_info$def
                                        # -- End function
	.type	GHCComp_bar_closure,@object # @GHCComp_bar_closure
	.data
	.globl	GHCComp_bar_closure
	.p2align	3
GHCComp_bar_closure:
	.quad	GHCComp_bar_info$def
	.size	GHCComp_bar_closure, 8

	.type	GHCComp_foo_closure,@object # @GHCComp_foo_closure
	.globl	GHCComp_foo_closure
	.p2align	3
GHCComp_foo_closure:
	.quad	GHCComp_foo_info$def
	.size	GHCComp_foo_closure, 8

	.type	GHCComp_zdtrModule4_bytes,@object # @GHCComp_zdtrModule4_bytes
	.section	.rodata,"a",@progbits
	.globl	GHCComp_zdtrModule4_bytes
GHCComp_zdtrModule4_bytes:
	.asciz	"main"
	.size	GHCComp_zdtrModule4_bytes, 5

	.type	GHCComp_zdtrModule3_closure,@object # @GHCComp_zdtrModule3_closure
	.data
	.globl	GHCComp_zdtrModule3_closure
	.p2align	3
GHCComp_zdtrModule3_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	GHCComp_zdtrModule4_bytes
	.size	GHCComp_zdtrModule3_closure, 16

	.type	GHCComp_zdtrModule2_bytes,@object # @GHCComp_zdtrModule2_bytes
	.section	.rodata,"a",@progbits
	.globl	GHCComp_zdtrModule2_bytes
GHCComp_zdtrModule2_bytes:
	.asciz	"GHCComp"
	.size	GHCComp_zdtrModule2_bytes, 8

	.type	GHCComp_zdtrModule1_closure,@object # @GHCComp_zdtrModule1_closure
	.data
	.globl	GHCComp_zdtrModule1_closure
	.p2align	3
GHCComp_zdtrModule1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	GHCComp_zdtrModule2_bytes
	.size	GHCComp_zdtrModule1_closure, 16

	.type	GHCComp_zdtrModule_closure,@object # @GHCComp_zdtrModule_closure
	.globl	GHCComp_zdtrModule_closure
	.p2align	4
GHCComp_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	GHCComp_zdtrModule3_closure+1
	.quad	GHCComp_zdtrModule1_closure+1
	.quad	3                       # 0x3
	.size	GHCComp_zdtrModule_closure, 32


	.globl	GHCComp_bar_info
.set GHCComp_bar_info, GHCComp_bar_info$def
	.globl	GHCComp_foo_info
.set GHCComp_foo_info, GHCComp_foo_info$def
	.section	".note.GNU-stack","",@progbits
