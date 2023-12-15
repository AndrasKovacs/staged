.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Foo_zdtczqNode3_bytes
.type Foo_zdtczqNode3_bytes, @object
Foo_zdtczqNode3_bytes:
	.string "'Node"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Foo_zdtczqLeaf3_bytes
.type Foo_zdtczqLeaf3_bytes, @object
Foo_zdtczqLeaf3_bytes:
	.string "'Leaf"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Foo_zdtcTree2_bytes
.type Foo_zdtcTree2_bytes, @object
Foo_zdtcTree2_bytes:
	.string "Tree"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Foo_zdtrModule2_bytes
.type Foo_zdtrModule2_bytes, @object
Foo_zdtrModule2_bytes:
	.string "Foo"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Foo_zdtrModule4_bytes
.type Foo_zdtrModule4_bytes, @object
Foo_zdtrModule4_bytes:
	.string "main"
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
.globl Foo_zdWLeaf_info
.type Foo_zdWLeaf_info, @function
Foo_zdWLeaf_info:
.LcCW:
	leaq -8(%rbp),%rax
	cmpq %r15,%rax
	jb .LcD0
.LcD1:
	movq $.LcCT_info,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne .LcCT
.LcCU:
	jmp *(%rbx)
.align 8
	.quad	0
	.long	30
	.long	0
.LcCT_info:
.LcCT:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcD4
.LcD3:
	movq 7(%rbx),%rax
	movq $Foo_Leaf_con_info,-8(%r12)
	movq %rax,(%r12)
	leaq -7(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
.LcD4:
	movq $16,904(%r13)
	jmp stg_gc_unpt_r1
.LcD0:
	leaq Foo_zdWLeaf_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Foo_zdWLeaf_info, .-Foo_zdWLeaf_info
.section .data
.align 8
.align 1
.globl Foo_zdWLeaf_closure
.type Foo_zdWLeaf_closure, @object
Foo_zdWLeaf_closure:
	.quad	Foo_zdWLeaf_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Foo_zdWNode_info
.type Foo_zdWNode_info, @function
Foo_zdWNode_info:
.LcDg:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcDp
.LcDq:
	movq $.LcDd_info,-16(%rbp)
	movq %r14,%rbx
	movq %rsi,-8(%rbp)
	addq $-16,%rbp
	testb $7,%bl
	jne .LcDd
.LcDe:
	jmp *(%rbx)
.align 8
	.quad	1
	.long	30
	.long	0
.LcDd_info:
.LcDd:
	movq $.LcDj_info,(%rbp)
	movq %rbx,%rax
	movq 8(%rbp),%rbx
	movq %rax,8(%rbp)
	testb $7,%bl
	jne .LcDj
.LcDk:
	jmp *(%rbx)
.align 8
	.quad	1
	.long	30
	.long	0
.LcDj_info:
.LcDj:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .LcDu
.LcDt:
	movq $Foo_Node_con_info,-16(%r12)
	movq 8(%rbp),%rax
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	leaq -14(%r12),%rbx
	addq $16,%rbp
	jmp *(%rbp)
.LcDu:
	movq $24,904(%r13)
	jmp stg_gc_unpt_r1
.LcDp:
	leaq Foo_zdWNode_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Foo_zdWNode_info, .-Foo_zdWNode_info
.section .data
.align 8
.align 1
.globl Foo_zdWNode_closure
.type Foo_zdWNode_closure, @object
Foo_zdWNode_closure:
	.quad	Foo_zdWNode_info
.section .data
.align 8
.align 1
.globl Foo_zdtrModule3_closure
.type Foo_zdtrModule3_closure, @object
Foo_zdtrModule3_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Foo_zdtrModule4_bytes
.section .data
.align 8
.align 1
.globl Foo_zdtrModule1_closure
.type Foo_zdtrModule1_closure, @object
Foo_zdtrModule1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Foo_zdtrModule2_bytes
.section .data
.align 8
.align 1
.globl Foo_zdtrModule_closure
.type Foo_zdtrModule_closure, @object
Foo_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	Foo_zdtrModule3_closure+1
	.quad	Foo_zdtrModule1_closure+1
	.quad	3
.section .data
.align 8
.align 1
.LrzR_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	ghczmprim_GHCziTypes_zdtcInt_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Foo_zdtcTree1_closure
.type Foo_zdtcTree1_closure, @object
Foo_zdtcTree1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Foo_zdtcTree2_bytes
.section .data
.align 8
.align 1
.globl Foo_zdtcTree_closure
.type Foo_zdtcTree_closure, @object
Foo_zdtcTree_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Foo_zdtrModule_closure+1
	.quad	Foo_zdtcTree1_closure+1
	.quad	ghczmprim_GHCziTypes_krepzdzt_closure+5
	.quad	2238336775069737953
	.quad	-2603241387390012070
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.LrzS_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	Foo_zdtcTree_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Foo_zdtczqLeaf1_closure
.type Foo_zdtczqLeaf1_closure, @object
Foo_zdtczqLeaf1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.LrzR_closure+1
	.quad	.LrzS_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Foo_zdtczqLeaf2_closure
.type Foo_zdtczqLeaf2_closure, @object
Foo_zdtczqLeaf2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Foo_zdtczqLeaf3_bytes
.section .data
.align 8
.align 1
.globl Foo_zdtczqLeaf_closure
.type Foo_zdtczqLeaf_closure, @object
Foo_zdtczqLeaf_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Foo_zdtrModule_closure+1
	.quad	Foo_zdtczqLeaf2_closure+1
	.quad	Foo_zdtczqLeaf1_closure+4
	.quad	4291319462912910996
	.quad	8897435281296772722
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.LrzT_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.LrzS_closure+1
	.quad	.LrzS_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Foo_zdtczqNode1_closure
.type Foo_zdtczqNode1_closure, @object
Foo_zdtczqNode1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.LrzS_closure+1
	.quad	.LrzT_closure+4
	.quad	3
.section .data
.align 8
.align 1
.globl Foo_zdtczqNode2_closure
.type Foo_zdtczqNode2_closure, @object
Foo_zdtczqNode2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Foo_zdtczqNode3_bytes
.section .data
.align 8
.align 1
.globl Foo_zdtczqNode_closure
.type Foo_zdtczqNode_closure, @object
Foo_zdtczqNode_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Foo_zdtrModule_closure+1
	.quad	Foo_zdtczqNode2_closure+1
	.quad	Foo_zdtczqNode1_closure+4
	.quad	-3143033580236110432
	.quad	1421417941602602442
	.quad	0
	.quad	3
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
.globl Foo_zdwfoo_info
.type Foo_zdwfoo_info, @function
Foo_zdwfoo_info:
.LcDY:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcDZ
.LcE0:
	movq %r14,%rax
	andl $7,%r14d
	cmpq $1,%r14
	je .LcE3
.LcDW:
	movq $.LcEg_info,-16(%rbp)
	movq 6(%rax),%r14
	movq 14(%rax),%rax
	movq %rax,-8(%rbp)
	addq $-16,%rbp
	jmp Foo_zdwfoo_info
.LcDZ:
	leaq Foo_zdwfoo_closure(%rip),%rbx
	jmp *-8(%r13)
.align 8
	.quad	0
	.long	30
	.long	0
.LcE2_info:
.LcE2:
	addq $8,%rbp
.LnED:
	movq %rbx,%rax
.LcE3:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcEb
.LcEa:
	movq 7(%rax),%rax
	addq $100,%rax
	movq $Foo_Leaf_con_info,-8(%r12)
	movq %rax,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.LcEb:
	movq $16,904(%r13)
	movq $.LcE2_info,-8(%rbp)
	movq %rax,%rbx
	addq $-8,%rbp
	jmp stg_gc_unpt_r1
.align 8
	.quad	1
	.long	30
	.long	0
.LcEg_info:
.LcEg:
	movq $.LcEk_info,(%rbp)
	movq 8(%rbp),%r14
	movq %rbx,8(%rbp)
	jmp Foo_zdwfoo_info
.align 8
	.quad	1
	.long	30
	.long	0
.LcEk_info:
.LcEk:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .LcEt
.LcEs:
	movq $Foo_Node_con_info,-16(%r12)
	movq 8(%rbp),%rax
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	leaq -14(%r12),%rbx
	addq $16,%rbp
	jmp *(%rbp)
.LcEt:
	movq $24,904(%r13)
	jmp stg_gc_unpt_r1
	.size Foo_zdwfoo_info, .-Foo_zdwfoo_info
.section .data
.align 8
.align 1
.globl Foo_zdwfoo_closure
.type Foo_zdwfoo_closure, @object
Foo_zdwfoo_closure:
	.quad	Foo_zdwfoo_info
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
.globl Foo_foo_info
.type Foo_foo_info, @function
Foo_foo_info:
.LcEM:
	leaq -8(%rbp),%rax
	cmpq %r15,%rax
	jb .LcEN
.LcEO:
	movq $.LcEJ_info,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne .LcEJ
.LcEK:
	jmp *(%rbx)
.align 8
	.quad	0
	.long	30
	.long	0
.LcEJ_info:
.LcEJ:
	movq %rbx,%r14
	addq $8,%rbp
	jmp Foo_zdwfoo_info
.LcEN:
	leaq Foo_foo_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Foo_foo_info, .-Foo_foo_info
.section .data
.align 8
.align 1
.globl Foo_foo_closure
.type Foo_foo_closure, @object
Foo_foo_closure:
	.quad	Foo_foo_info
.section .text
.align 8
.align 8
	.quad	4294967300
	.quad	0
	.long	14
	.long	0
.globl Foo_Leaf_info
.type Foo_Leaf_info, @function
Foo_Leaf_info:
.LcF1:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcF5
.LcF4:
	movq $Foo_Leaf_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.LcF5:
	movq $16,904(%r13)
	leaq Foo_Leaf_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Foo_Leaf_info, .-Foo_Leaf_info
.section .data
.align 8
.align 1
.globl Foo_Leaf_closure
.type Foo_Leaf_closure, @object
Foo_Leaf_closure:
	.quad	Foo_Leaf_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Foo_Node_info
.type Foo_Node_info, @function
Foo_Node_info:
.LcFg:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcFp
.LcFq:
	movq $.LcFd_info,-16(%rbp)
	movq %r14,%rbx
	movq %rsi,-8(%rbp)
	addq $-16,%rbp
	testb $7,%bl
	jne .LcFd
.LcFe:
	jmp *(%rbx)
.align 8
	.quad	1
	.long	30
	.long	0
.LcFd_info:
.LcFd:
	movq $.LcFj_info,(%rbp)
	movq %rbx,%rax
	movq 8(%rbp),%rbx
	movq %rax,8(%rbp)
	testb $7,%bl
	jne .LcFj
.LcFk:
	jmp *(%rbx)
.align 8
	.quad	1
	.long	30
	.long	0
.LcFj_info:
.LcFj:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .LcFu
.LcFt:
	movq $Foo_Node_con_info,-16(%r12)
	movq 8(%rbp),%rax
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	leaq -14(%r12),%rbx
	addq $16,%rbp
	jmp *(%rbp)
.LcFu:
	movq $24,904(%r13)
	jmp stg_gc_unpt_r1
.LcFp:
	leaq Foo_Node_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Foo_Node_info, .-Foo_Node_info
.section .data
.align 8
.align 1
.globl Foo_Node_closure
.type Foo_Node_closure, @object
Foo_Node_closure:
	.quad	Foo_Node_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
iFB_str:
	.string "main:Foo.Leaf"
.section .text
.align 8
.align 8
	.long	iFB_str-(Foo_Leaf_con_info)+0
	.long	0
	.quad	4294967296
	.long	3
	.long	0
.globl Foo_Leaf_con_info
.type Foo_Leaf_con_info, @object
Foo_Leaf_con_info:
.LcFA:
	incq %rbx
	jmp *(%rbp)
	.size Foo_Leaf_con_info, .-Foo_Leaf_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
iFG_str:
	.string "main:Foo.Node"
.section .text
.align 8
.align 8
	.long	iFG_str-(Foo_Node_con_info)+0
	.long	0
	.quad	2
	.long	4
	.long	1
.globl Foo_Node_con_info
.type Foo_Node_con_info, @object
Foo_Node_con_info:
.LcFF:
	addq $2,%rbx
	jmp *(%rbp)
	.size Foo_Node_con_info, .-Foo_Node_con_info
.section .note.GNU-stack,"",@progbits
.ident "GHC 9.4.4"


