	.text
	.file	"ghc_1.ll"
	.globl	Notes_foo_info$def              # -- Begin function Notes_foo_info$def
	.p2align	4, 0x90
	.type	Notes_foo_info$def,@object
	.quad	4294967301                      # @"Notes_foo_info$def"
                                        # 0x100000005
	.quad	0                               # 0x0
	.long	14                              # 0xe
	.long	0                               # 0x0
Notes_foo_info$def:
# %bb.0:                                # %nDj
	leaq	-16(%rbp), %rdx
	cmpq	%r15, %rdx
	jb	.LBB0_1
	.p2align	4, 0x90
.LBB0_2:                                # %cCX
                                        # =>This Inner Loop Header: Depth=1
	movl	%r14d, %eax
	andl	$7, %eax
	cmpl	$2, %eax
	jne	.LBB0_5
# %bb.3:                                # %cCT
                                        #   in Loop: Header=BB0_2 Depth=1
	leaq	-16(%rbp), %rax
	movq	$cD7_info$def, -16(%rbp)
	movq	6(%r14), %rcx
	movq	14(%r14), %rsi
	movq	%rsi, -8(%rbp)
	addq	$-16, %rdx
	movq	%rax, %rbp
	movq	%rcx, %r14
	cmpq	%r15, %rdx
	jae	.LBB0_2
.LBB0_4:                                # %cCW
	movq	-8(%r13), %rdx
	movl	$Notes_foo_closure$def, %ebx
	movq	%rax, %rbp
	movq	%rcx, %r14
	jmpq	*%rdx                           # TAILCALL
.LBB0_5:                                # %cCS
	movq	(%rbp), %rax
	movq	Notes_Nil_closure@GOTPCREL(%rip), %rbx
	addq	$1, %rbx
	jmpq	*%rax                           # TAILCALL
.LBB0_1:
	movq	%rbp, %rax
	movq	%r14, %rcx
	jmp	.LBB0_4
.Lfunc_end0:
	.size	Notes_foo_info$def, .Lfunc_end0-Notes_foo_info$def
                                        # -- End function
	.p2align	4, 0x90                         # -- Begin function cD7_info$def
	.type	cD7_info$def,@object
	.quad	65                              # @"cD7_info$def"
                                        # 0x41
	.long	30                              # 0x1e
	.long	0                               # 0x0
cD7_info$def:
# %bb.0:                                # %nE9
	movq	%r12, %rax
	addq	$24, %r12
	cmpq	%r12, 856(%r13)
	jb	.LBB1_2
# %bb.1:                                # %cDh
	movq	Notes_Cons_con_info@GOTPCREL(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	%rbx, 16(%rax)
	movq	8(%rbp), %rcx
	addq	$10, %rcx
	movq	%rcx, 24(%rax)
	leaq	-14(%r12), %rbx
	movq	16(%rbp), %rax
	addq	$16, %rbp
	jmpq	*%rax                           # TAILCALL
.LBB1_2:                                # %cDi
	movq	$24, 904(%r13)
	jmp	stg_gc_unpt_r1@PLT              # TAILCALL
.Lfunc_end1:
	.size	cD7_info$def, .Lfunc_end1-cD7_info$def
                                        # -- End function
	.globl	Notes_Cons_info$def             # -- Begin function Notes_Cons_info$def
	.p2align	4, 0x90
	.type	Notes_Cons_info$def,@object
	.quad	8589934605                      # @"Notes_Cons_info$def"
                                        # 0x20000000d
	.quad	0                               # 0x0
	.long	14                              # 0xe
	.long	0                               # 0x0
Notes_Cons_info$def:
# %bb.0:                                # %nF3
	movq	%r12, %rax
	addq	$24, %r12
	cmpq	%r12, 856(%r13)
	jb	.LBB2_2
# %bb.1:                                # %cF1
	movq	Notes_Cons_con_info@GOTPCREL(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	%rsi, 16(%rax)
	movq	%r14, 24(%rax)
	leaq	-14(%r12), %rbx
	movq	(%rbp), %rax
	jmpq	*%rax                           # TAILCALL
.LBB2_2:                                # %cF2
	movq	$24, 904(%r13)
	movq	-8(%r13), %rax
	movl	$Notes_Cons_closure$def, %ebx
	jmpq	*%rax                           # TAILCALL
.Lfunc_end2:
	.size	Notes_Cons_info$def, .Lfunc_end2-Notes_Cons_info$def
                                        # -- End function
	.globl	Notes_Nil_con_info$def          # -- Begin function Notes_Nil_con_info$def
	.p2align	4, 0x90
	.type	Notes_Nil_con_info$def,@object
	.quad	iFF_str$def-Notes_Nil_con_info$def # @"Notes_Nil_con_info$def"
	.quad	4294967296                      # 0x100000000
	.long	3                               # 0x3
	.long	0                               # 0x0
Notes_Nil_con_info$def:
# %bb.0:                                # %nFG
	addq	$1, %rbx
	movq	(%rbp), %rax
	jmpq	*%rax                           # TAILCALL
.Lfunc_end3:
	.size	Notes_Nil_con_info$def, .Lfunc_end3-Notes_Nil_con_info$def
                                        # -- End function
	.globl	Notes_Cons_con_info$def         # -- Begin function Notes_Cons_con_info$def
	.p2align	4, 0x90
	.type	Notes_Cons_con_info$def,@object
	.quad	iFQ_str$def-Notes_Cons_con_info$def # @"Notes_Cons_con_info$def"
	.quad	4294967297                      # 0x100000001
	.long	5                               # 0x5
	.long	1                               # 0x1
Notes_Cons_con_info$def:
# %bb.0:                                # %nFR
	addq	$2, %rbx
	movq	(%rbp), %rax
	jmpq	*%rax                           # TAILCALL
.Lfunc_end4:
	.size	Notes_Cons_con_info$def, .Lfunc_end4-Notes_Cons_con_info$def
                                        # -- End function
	.type	Notes_zdtczqCons3_bytes$def,@object # @"Notes_zdtczqCons3_bytes$def"
	.section	".rodata.Notes_zdtczqCons3_bytes$def","aR",@progbits
Notes_zdtczqCons3_bytes$def:
	.asciz	"'Cons"
	.size	Notes_zdtczqCons3_bytes$def, 6

	.type	Notes_zdtczqNil3_bytes$def,@object # @"Notes_zdtczqNil3_bytes$def"
	.section	".rodata.Notes_zdtczqNil3_bytes$def","aR",@progbits
Notes_zdtczqNil3_bytes$def:
	.asciz	"'Nil"
	.size	Notes_zdtczqNil3_bytes$def, 5

	.type	Notes_zdtcList3_bytes$def,@object # @"Notes_zdtcList3_bytes$def"
	.section	".rodata.Notes_zdtcList3_bytes$def","aR",@progbits
Notes_zdtcList3_bytes$def:
	.asciz	"List"
	.size	Notes_zdtcList3_bytes$def, 5

	.type	Notes_zdtrModule2_bytes$def,@object # @"Notes_zdtrModule2_bytes$def"
	.section	".rodata.Notes_zdtrModule2_bytes$def","aR",@progbits
Notes_zdtrModule2_bytes$def:
	.asciz	"Notes"
	.size	Notes_zdtrModule2_bytes$def, 6

	.type	Notes_zdtrModule4_bytes$def,@object # @"Notes_zdtrModule4_bytes$def"
	.section	".rodata.Notes_zdtrModule4_bytes$def","aR",@progbits
Notes_zdtrModule4_bytes$def:
	.asciz	"main"
	.size	Notes_zdtrModule4_bytes$def, 5

	.type	Notes_foo_closure$def,@object   # @"Notes_foo_closure$def"
	.section	".data.Notes_foo_closure$def","awR",@progbits
	.p2align	3
Notes_foo_closure$def:
	.quad	Notes_foo_info$def
	.size	Notes_foo_closure$def, 8

	.type	Notes_zdtrModule3_closure$def,@object # @"Notes_zdtrModule3_closure$def"
	.section	".data.Notes_zdtrModule3_closure$def","awR",@progbits
	.p2align	3
Notes_zdtrModule3_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Notes_zdtrModule4_bytes$def
	.size	Notes_zdtrModule3_closure$def, 16

	.type	Notes_zdtrModule1_closure$def,@object # @"Notes_zdtrModule1_closure$def"
	.section	".data.Notes_zdtrModule1_closure$def","awR",@progbits
	.p2align	3
Notes_zdtrModule1_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Notes_zdtrModule2_bytes$def
	.size	Notes_zdtrModule1_closure$def, 16

	.type	Notes_zdtrModule_closure$def,@object # @"Notes_zdtrModule_closure$def"
	.section	".data.Notes_zdtrModule_closure$def","awR",@progbits
	.p2align	4
Notes_zdtrModule_closure$def:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	Notes_zdtrModule3_closure$def+1
	.quad	Notes_zdtrModule1_closure$def+1
	.quad	3                               # 0x3
	.size	Notes_zdtrModule_closure$def, 32

	.type	rzH_closure$def,@object         # @"rzH_closure$def"
	.section	".data.rzH_closure$def","awR",@progbits
	.p2align	4
rzH_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	ghczmprim_GHCziTypes_zdtcIntzh_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3                               # 0x3
	.size	rzH_closure$def, 32

	.type	rzI_closure$def,@object         # @"rzI_closure$def"
	.section	".data.rzI_closure$def","awR",@progbits
	.p2align	4
rzI_closure$def:
	.quad	ghczmprim_GHCziTypes_BoxedRep_con_info
	.quad	ghczmprim_GHCziTypes_Unlifted_closure+2
	.quad	3                               # 0x3
	.size	rzI_closure$def, 24

	.type	Notes_zdtcList1_closure$def,@object # @"Notes_zdtcList1_closure$def"
	.section	".data.Notes_zdtcList1_closure$def","awR",@progbits
	.p2align	4
Notes_zdtcList1_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepTYPE_con_info
	.quad	rzI_closure$def+4
	.quad	3                               # 0x3
	.size	Notes_zdtcList1_closure$def, 24

	.type	Notes_zdtcList2_closure$def,@object # @"Notes_zdtcList2_closure$def"
	.section	".data.Notes_zdtcList2_closure$def","awR",@progbits
	.p2align	3
Notes_zdtcList2_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Notes_zdtcList3_bytes$def
	.size	Notes_zdtcList2_closure$def, 16

	.type	Notes_zdtcList_closure$def,@object # @"Notes_zdtcList_closure$def"
	.section	".data.Notes_zdtcList_closure$def","awR",@progbits
	.p2align	4
Notes_zdtcList_closure$def:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Notes_zdtrModule_closure$def+1
	.quad	Notes_zdtcList2_closure$def+1
	.quad	Notes_zdtcList1_closure$def+5
	.quad	-6979571512970097075            # 0x9f2394b034fb4e4d
	.quad	-2421323013856044972            # 0xde65bb8c30a1d854
	.quad	0                               # 0x0
	.quad	3                               # 0x3
	.size	Notes_zdtcList_closure$def, 64

	.type	Notes_zdtczqNil1_closure$def,@object # @"Notes_zdtczqNil1_closure$def"
	.section	".data.Notes_zdtczqNil1_closure$def","awR",@progbits
	.p2align	4
Notes_zdtczqNil1_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	Notes_zdtcList_closure$def+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3                               # 0x3
	.size	Notes_zdtczqNil1_closure$def, 32

	.type	Notes_zdtczqNil2_closure$def,@object # @"Notes_zdtczqNil2_closure$def"
	.section	".data.Notes_zdtczqNil2_closure$def","awR",@progbits
	.p2align	3
Notes_zdtczqNil2_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Notes_zdtczqNil3_bytes$def
	.size	Notes_zdtczqNil2_closure$def, 16

	.type	Notes_zdtczqNil_closure$def,@object # @"Notes_zdtczqNil_closure$def"
	.section	".data.Notes_zdtczqNil_closure$def","awR",@progbits
	.p2align	4
Notes_zdtczqNil_closure$def:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Notes_zdtrModule_closure$def+1
	.quad	Notes_zdtczqNil2_closure$def+1
	.quad	Notes_zdtczqNil1_closure$def+1
	.quad	-2002203830244736031            # 0xe436be397a5603e1
	.quad	-2403186628685111318            # 0xdea62a7e6c415fea
	.quad	0                               # 0x0
	.quad	3                               # 0x3
	.size	Notes_zdtczqNil_closure$def, 64

	.type	rzJ_closure$def,@object         # @"rzJ_closure$def"
	.section	".data.rzJ_closure$def","awR",@progbits
	.p2align	4
rzJ_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	Notes_zdtczqNil1_closure$def+1
	.quad	Notes_zdtczqNil1_closure$def+1
	.quad	3                               # 0x3
	.size	rzJ_closure$def, 32

	.type	Notes_zdtczqCons1_closure$def,@object # @"Notes_zdtczqCons1_closure$def"
	.section	".data.Notes_zdtczqCons1_closure$def","awR",@progbits
	.p2align	4
Notes_zdtczqCons1_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	rzH_closure$def+1
	.quad	rzJ_closure$def+4
	.quad	3                               # 0x3
	.size	Notes_zdtczqCons1_closure$def, 32

	.type	Notes_zdtczqCons2_closure$def,@object # @"Notes_zdtczqCons2_closure$def"
	.section	".data.Notes_zdtczqCons2_closure$def","awR",@progbits
	.p2align	3
Notes_zdtczqCons2_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Notes_zdtczqCons3_bytes$def
	.size	Notes_zdtczqCons2_closure$def, 16

	.type	Notes_zdtczqCons_closure$def,@object # @"Notes_zdtczqCons_closure$def"
	.section	".data.Notes_zdtczqCons_closure$def","awR",@progbits
	.p2align	4
Notes_zdtczqCons_closure$def:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Notes_zdtrModule_closure$def+1
	.quad	Notes_zdtczqCons2_closure$def+1
	.quad	Notes_zdtczqCons1_closure$def+4
	.quad	3411396880282939473             # 0x2f57b7636523f051
	.quad	1207782852456034779             # 0x10c2e81aee7331db
	.quad	0                               # 0x0
	.quad	3                               # 0x3
	.size	Notes_zdtczqCons_closure$def, 64

	.type	Notes_Nil_closure$def,@object   # @"Notes_Nil_closure$def"
	.section	".data.Notes_Nil_closure$def","awR",@progbits
	.p2align	3
Notes_Nil_closure$def:
	.quad	Notes_Nil_con_info
	.size	Notes_Nil_closure$def, 8

	.type	Notes_Cons_closure$def,@object  # @"Notes_Cons_closure$def"
	.section	".data.Notes_Cons_closure$def","awR",@progbits
	.p2align	3
Notes_Cons_closure$def:
	.quad	Notes_Cons_info$def
	.size	Notes_Cons_closure$def, 8

	.type	iFF_str$def,@object             # @"iFF_str$def"
	.section	".rodata.iFF_str$def","aR",@progbits
iFF_str$def:
	.asciz	"main:Notes.Nil"
	.size	iFF_str$def, 15

	.type	iFQ_str$def,@object             # @"iFQ_str$def"
	.section	".rodata.iFQ_str$def","aR",@progbits
iFQ_str$def:
	.asciz	"main:Notes.Cons"
	.size	iFQ_str$def, 16

	.globl	Notes_zdtczqCons3_bytes
.set Notes_zdtczqCons3_bytes, Notes_zdtczqCons3_bytes$def
	.globl	Notes_zdtczqNil3_bytes
.set Notes_zdtczqNil3_bytes, Notes_zdtczqNil3_bytes$def
	.globl	Notes_zdtcList3_bytes
.set Notes_zdtcList3_bytes, Notes_zdtcList3_bytes$def
	.globl	Notes_zdtrModule2_bytes
.set Notes_zdtrModule2_bytes, Notes_zdtrModule2_bytes$def
	.globl	Notes_zdtrModule4_bytes
.set Notes_zdtrModule4_bytes, Notes_zdtrModule4_bytes$def
	.globl	Notes_foo_closure
.set Notes_foo_closure, Notes_foo_closure$def
	.globl	Notes_foo_info
	.type	Notes_foo_info,@object
.set Notes_foo_info, Notes_foo_info$def
	.globl	Notes_zdtrModule3_closure
.set Notes_zdtrModule3_closure, Notes_zdtrModule3_closure$def
	.globl	Notes_zdtrModule1_closure
.set Notes_zdtrModule1_closure, Notes_zdtrModule1_closure$def
	.globl	Notes_zdtrModule_closure
.set Notes_zdtrModule_closure, Notes_zdtrModule_closure$def
	.globl	Notes_zdtcList1_closure
.set Notes_zdtcList1_closure, Notes_zdtcList1_closure$def
	.globl	Notes_zdtcList2_closure
.set Notes_zdtcList2_closure, Notes_zdtcList2_closure$def
	.globl	Notes_zdtcList_closure
.set Notes_zdtcList_closure, Notes_zdtcList_closure$def
	.globl	Notes_zdtczqNil1_closure
.set Notes_zdtczqNil1_closure, Notes_zdtczqNil1_closure$def
	.globl	Notes_zdtczqNil2_closure
.set Notes_zdtczqNil2_closure, Notes_zdtczqNil2_closure$def
	.globl	Notes_zdtczqNil_closure
.set Notes_zdtczqNil_closure, Notes_zdtczqNil_closure$def
	.globl	Notes_zdtczqCons1_closure
.set Notes_zdtczqCons1_closure, Notes_zdtczqCons1_closure$def
	.globl	Notes_zdtczqCons2_closure
.set Notes_zdtczqCons2_closure, Notes_zdtczqCons2_closure$def
	.globl	Notes_zdtczqCons_closure
.set Notes_zdtczqCons_closure, Notes_zdtczqCons_closure$def
	.globl	Notes_Nil_closure
.set Notes_Nil_closure, Notes_Nil_closure$def
	.globl	Notes_Cons_closure
.set Notes_Cons_closure, Notes_Cons_closure$def
	.globl	Notes_Cons_info
	.type	Notes_Cons_info,@object
.set Notes_Cons_info, Notes_Cons_info$def
	.globl	Notes_Nil_con_info
	.type	Notes_Nil_con_info,@object
.set Notes_Nil_con_info, Notes_Nil_con_info$def
	.globl	Notes_Cons_con_info
	.type	Notes_Cons_con_info,@object
.set Notes_Cons_con_info, Notes_Cons_con_info$def
	.section	".note.GNU-stack","",@progbits
