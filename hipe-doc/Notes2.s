	.text
	.file	"ghc_1.ll"
	.globl	Notes2_foo_info$def             # -- Begin function Notes2_foo_info$def
	.p2align	4, 0x90
	.type	Notes2_foo_info$def,@object
	.quad	4294967300                      # @"Notes2_foo_info$def"
                                        # 0x100000004
	.quad	0                               # 0x0
	.long	14                              # 0xe
	.long	0                               # 0x0
Notes2_foo_info$def:
# %bb.0:                                # %nBN
	movq	%r14, %rbx
	movq	(%rbp), %rax
	jmpq	*%rax                           # TAILCALL
.Lfunc_end0:
	.size	Notes2_foo_info$def, .Lfunc_end0-Notes2_foo_info$def
                                        # -- End function
	.type	Notes2_zdtrModule2_bytes$def,@object # @"Notes2_zdtrModule2_bytes$def"
	.section	".rodata.Notes2_zdtrModule2_bytes$def","aR",@progbits
Notes2_zdtrModule2_bytes$def:
	.asciz	"Notes2"
	.size	Notes2_zdtrModule2_bytes$def, 7

	.type	Notes2_zdtrModule4_bytes$def,@object # @"Notes2_zdtrModule4_bytes$def"
	.section	".rodata.Notes2_zdtrModule4_bytes$def","aR",@progbits
Notes2_zdtrModule4_bytes$def:
	.asciz	"main"
	.size	Notes2_zdtrModule4_bytes$def, 5

	.type	Notes2_foo_closure$def,@object  # @"Notes2_foo_closure$def"
	.section	".data.Notes2_foo_closure$def","awR",@progbits
	.p2align	3
Notes2_foo_closure$def:
	.quad	Notes2_foo_info$def
	.size	Notes2_foo_closure$def, 8

	.type	Notes2_zdtrModule3_closure$def,@object # @"Notes2_zdtrModule3_closure$def"
	.section	".data.Notes2_zdtrModule3_closure$def","awR",@progbits
	.p2align	3
Notes2_zdtrModule3_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Notes2_zdtrModule4_bytes$def
	.size	Notes2_zdtrModule3_closure$def, 16

	.type	Notes2_zdtrModule1_closure$def,@object # @"Notes2_zdtrModule1_closure$def"
	.section	".data.Notes2_zdtrModule1_closure$def","awR",@progbits
	.p2align	3
Notes2_zdtrModule1_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Notes2_zdtrModule2_bytes$def
	.size	Notes2_zdtrModule1_closure$def, 16

	.type	Notes2_zdtrModule_closure$def,@object # @"Notes2_zdtrModule_closure$def"
	.section	".data.Notes2_zdtrModule_closure$def","awR",@progbits
	.p2align	4
Notes2_zdtrModule_closure$def:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	Notes2_zdtrModule3_closure$def+1
	.quad	Notes2_zdtrModule1_closure$def+1
	.quad	3                               # 0x3
	.size	Notes2_zdtrModule_closure$def, 32

	.globl	Notes2_zdtrModule2_bytes
.set Notes2_zdtrModule2_bytes, Notes2_zdtrModule2_bytes$def
	.globl	Notes2_zdtrModule4_bytes
.set Notes2_zdtrModule4_bytes, Notes2_zdtrModule4_bytes$def
	.globl	Notes2_foo_closure
.set Notes2_foo_closure, Notes2_foo_closure$def
	.globl	Notes2_foo_info
	.type	Notes2_foo_info,@object
.set Notes2_foo_info, Notes2_foo_info$def
	.globl	Notes2_zdtrModule3_closure
.set Notes2_zdtrModule3_closure, Notes2_zdtrModule3_closure$def
	.globl	Notes2_zdtrModule1_closure
.set Notes2_zdtrModule1_closure, Notes2_zdtrModule1_closure$def
	.globl	Notes2_zdtrModule_closure
.set Notes2_zdtrModule_closure, Notes2_zdtrModule_closure$def
	.section	".note.GNU-stack","",@progbits
