.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.Lr12p_bytes:
	.string "Bench.hs"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.Lr12l_bytes:
	.string "undefined"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqUBool3_bytes
.type Bench_zdtczqUBool3_bytes, @object
Bench_zdtczqUBool3_bytes:
	.string "'UBool"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtcUBool3_bytes
.type Bench_zdtcUBool3_bytes, @object
Bench_zdtcUBool3_bytes:
	.string "UBool"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqVVar3_bytes
.type Bench_zdtczqVVar3_bytes, @object
Bench_zdtczqVVar3_bytes:
	.string "'VVar"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqVApp3_bytes
.type Bench_zdtczqVApp3_bytes, @object
Bench_zdtczqVApp3_bytes:
	.string "'VApp"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqVLam3_bytes
.type Bench_zdtczqVLam3_bytes, @object
Bench_zdtczqVLam3_bytes:
	.string "'VLam"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqCons3_bytes
.type Bench_zdtczqCons3_bytes, @object
Bench_zdtczqCons3_bytes:
	.string "'Cons"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtcVal2_bytes
.type Bench_zdtcVal2_bytes, @object
Bench_zdtcVal2_bytes:
	.string "Val"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqNil3_bytes
.type Bench_zdtczqNil3_bytes, @object
Bench_zdtczqNil3_bytes:
	.string "'Nil"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtcEnv3_bytes
.type Bench_zdtcEnv3_bytes, @object
Bench_zdtcEnv3_bytes:
	.string "Env"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqVar3_bytes
.type Bench_zdtczqVar3_bytes, @object
Bench_zdtczqVar3_bytes:
	.string "'Var"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqApp3_bytes
.type Bench_zdtczqApp3_bytes, @object
Bench_zdtczqApp3_bytes:
	.string "'App"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtczqLam3_bytes
.type Bench_zdtczqLam3_bytes, @object
Bench_zdtczqLam3_bytes:
	.string "'Lam"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtcTm2_bytes
.type Bench_zdtcTm2_bytes, @object
Bench_zdtcTm2_bytes:
	.string "Tm"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtrModule2_bytes
.type Bench_zdtrModule2_bytes, @object
Bench_zdtrModule2_bytes:
	.string "Bench"
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl Bench_zdtrModule4_bytes
.type Bench_zdtrModule4_bytes, @object
Bench_zdtrModule4_bytes:
	.string "main"
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbUTrue1_info
.type Bench_zdbUTrue1_info, @function
Bench_zdbUTrue1_info:
.Lc17q:
	movl $1,%ebx
	jmp *(%rbp)
	.size Bench_zdbUTrue1_info, .-Bench_zdbUTrue1_info
.section .data
.align 8
.align 1
.globl Bench_zdbUTrue1_closure
.type Bench_zdbUTrue1_closure, @object
Bench_zdbUTrue1_closure:
	.quad	Bench_zdbUTrue1_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbUTrue_info
.type Bench_zdbUTrue_info, @function
Bench_zdbUTrue_info:
.Lc17A:
	jmp Bench_zdbUTrue1_info
	.size Bench_zdbUTrue_info, .-Bench_zdbUTrue_info
.section .data
.align 8
.align 1
.globl Bench_zdbUTrue_closure
.type Bench_zdbUTrue_closure, @object
Bench_zdbUTrue_closure:
	.quad	Bench_zdbUTrue_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbUFalse1_info
.type Bench_zdbUFalse1_info, @function
Bench_zdbUFalse1_info:
.Lc17K:
	xorl %ebx,%ebx
	jmp *(%rbp)
	.size Bench_zdbUFalse1_info, .-Bench_zdbUFalse1_info
.section .data
.align 8
.align 1
.globl Bench_zdbUFalse1_closure
.type Bench_zdbUFalse1_closure, @object
Bench_zdbUFalse1_closure:
	.quad	Bench_zdbUFalse1_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbUFalse_info
.type Bench_zdbUFalse_info, @function
Bench_zdbUFalse_info:
.Lc17U:
	jmp Bench_zdbUFalse1_info
	.size Bench_zdbUFalse_info, .-Bench_zdbUFalse_info
.section .data
.align 8
.align 1
.globl Bench_zdbUFalse_closure
.type Bench_zdbUFalse_closure, @object
Bench_zdbUFalse_closure:
	.quad	Bench_zdbUFalse_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbV0_info
.type Bench_zdbV0_info, @function
Bench_zdbV0_info:
.Lc185:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc189
.Lc188:
	movq $Bench_Var_con_info,-8(%r12)
	movq $0,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc189:
	movq $16,904(%r13)
	leaq Bench_zdbV0_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_zdbV0_info, .-Bench_zdbV0_info
.section .data
.align 8
.align 1
.globl Bench_zdbV0_closure
.type Bench_zdbV0_closure, @object
Bench_zdbV0_closure:
	.quad	Bench_zdbV0_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbV1_info
.type Bench_zdbV1_info, @function
Bench_zdbV1_info:
.Lc18i:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc18m
.Lc18l:
	movq $Bench_Var_con_info,-8(%r12)
	movq $1,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc18m:
	movq $16,904(%r13)
	leaq Bench_zdbV1_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_zdbV1_info, .-Bench_zdbV1_info
.section .data
.align 8
.align 1
.globl Bench_zdbV1_closure
.type Bench_zdbV1_closure, @object
Bench_zdbV1_closure:
	.quad	Bench_zdbV1_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbV2_info
.type Bench_zdbV2_info, @function
Bench_zdbV2_info:
.Lc18v:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc18z
.Lc18y:
	movq $Bench_Var_con_info,-8(%r12)
	movq $2,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc18z:
	movq $16,904(%r13)
	leaq Bench_zdbV2_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_zdbV2_info, .-Bench_zdbV2_info
.section .data
.align 8
.align 1
.globl Bench_zdbV2_closure
.type Bench_zdbV2_closure, @object
Bench_zdbV2_closure:
	.quad	Bench_zdbV2_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbV3_info
.type Bench_zdbV3_info, @function
Bench_zdbV3_info:
.Lc18I:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc18M
.Lc18L:
	movq $Bench_Var_con_info,-8(%r12)
	movq $3,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc18M:
	movq $16,904(%r13)
	leaq Bench_zdbV3_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_zdbV3_info, .-Bench_zdbV3_info
.section .data
.align 8
.align 1
.globl Bench_zdbV3_closure
.type Bench_zdbV3_closure, @object
Bench_zdbV3_closure:
	.quad	Bench_zdbV3_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbV4_info
.type Bench_zdbV4_info, @function
Bench_zdbV4_info:
.Lc18V:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc18Z
.Lc18Y:
	movq $Bench_Var_con_info,-8(%r12)
	movq $4,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc18Z:
	movq $16,904(%r13)
	leaq Bench_zdbV4_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_zdbV4_info, .-Bench_zdbV4_info
.section .data
.align 8
.align 1
.globl Bench_zdbV4_closure
.type Bench_zdbV4_closure, @object
Bench_zdbV4_closure:
	.quad	Bench_zdbV4_info
.section .text
.align 8
.align 8
	.quad	4294967299
	.quad	0
	.long	14
	.long	0
.globl Bench_zdbV5_info
.type Bench_zdbV5_info, @function
Bench_zdbV5_info:
.Lc198:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc19c
.Lc19b:
	movq $Bench_Var_con_info,-8(%r12)
	movq $5,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc19c:
	movq $16,904(%r13)
	leaq Bench_zdbV5_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_zdbV5_info, .-Bench_zdbV5_info
.section .data
.align 8
.align 1
.globl Bench_zdbV5_closure
.type Bench_zdbV5_closure, @object
Bench_zdbV5_closure:
	.quad	Bench_zdbV5_info
.section .text
.align 8
.align 8
	.quad	12884901907
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmUTrue_info
.type Bench_zdmUTrue_info, @function
Bench_zdmUTrue_info:
.Lc19o:
	cmpq $1,%r14
	jne .Lc19m
.Lc19n:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc19m:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmUTrue_info, .-Bench_zdmUTrue_info
.section .data
.align 8
.align 1
.globl Bench_zdmUTrue_closure
.type Bench_zdmUTrue_closure, @object
Bench_zdmUTrue_closure:
	.quad	Bench_zdmUTrue_info
.section .text
.align 8
.align 8
	.quad	8589934604
	.quad	0
	.long	14
	.long	0
.globl Bench_uand_info
.type Bench_uand_info, @function
Bench_uand_info:
.Lc19y:
	movq %rsi,%rbx
	jmp *(%rbp)
	.size Bench_uand_info, .-Bench_uand_info
.section .data
.align 8
.align 1
.globl Bench_uand_closure
.type Bench_uand_closure, @object
Bench_uand_closure:
	.quad	Bench_uand_info
.section .text
.align 8
.align 8
	.quad	12884901907
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmUFalse_info
.type Bench_zdmUFalse_info, @function
Bench_zdmUFalse_info:
.Lc19M:
	testq %r14,%r14
	jne .Lc19K
.Lc19L:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc19K:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmUFalse_info, .-Bench_zdmUFalse_info
.section .data
.align 8
.align 1
.globl Bench_zdmUFalse_closure
.type Bench_zdmUFalse_closure, @object
Bench_zdmUFalse_closure:
	.quad	Bench_zdmUFalse_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Bench_zdzd_info
.type Bench_zdzd_info, @function
Bench_zdzd_info:
.Lc19X:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1a1
.Lc1a0:
	movq $Bench_App_con_info,-16(%r12)
	movq %r14,-8(%r12)
	movq %rsi,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
.Lc1a1:
	movq $24,904(%r13)
	leaq Bench_zdzd_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_zdzd_info, .-Bench_zdzd_info
.section .data
.align 8
.align 1
.globl Bench_zdzd_closure
.type Bench_zdzd_closure, @object
Bench_zdzd_closure:
	.quad	Bench_zdzd_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Bench_letzu_info
.type Bench_letzu_info, @function
Bench_letzu_info:
.Lc1ab:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1ah
.Lc1ai:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1ak
.Lc1aj:
	movq $Bench_Lam_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -5(%r12),%rbx
.Lc1ae:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1an
.Lc1am:
	movq $Bench_App_con_info,-16(%r12)
	movq %rbx,-8(%r12)
	movq %rsi,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
.Lc1an:
	movq $24,904(%r13)
	movq $.Lc1ad_info,-16(%rbp)
	movq %rsi,-8(%rbp)
	addq $-16,%rbp
	jmp stg_gc_unpt_r1
.Lc1ak:
	movq $16,904(%r13)
.Lc1ah:
	leaq Bench_letzu_closure(%rip),%rbx
	jmp *-8(%r13)
.align 8
	.quad	1
	.long	30
	.long	0
.Lc1ad_info:
.Lc1ad:
	movq 8(%rbp),%rsi
	addq $16,%rbp
	jmp .Lc1ae
	.size Bench_letzu_info, .-Bench_letzu_info
.section .data
.align 8
.align 1
.globl Bench_letzu_closure
.type Bench_letzu_closure, @object
Bench_letzu_closure:
	.quad	Bench_letzu_info
.section .text
.align 8
.align 8
	.quad	12884901911
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmV0_info
.type Bench_zdmV0_info, @function
Bench_zdmV0_info:
.Lc1aC:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	jne .Lc1az
.Lc1aA:
	cmpq $0,7(%r14)
	jne .Lc1az
.Lc1aS:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc1az:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmV0_info, .-Bench_zdmV0_info
.section .data
.align 8
.align 1
.globl Bench_zdmV0_closure
.type Bench_zdmV0_closure, @object
Bench_zdmV0_closure:
	.quad	Bench_zdmV0_info
.section .text
.align 8
.align 8
	.quad	12884901911
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmV1_info
.type Bench_zdmV1_info, @function
Bench_zdmV1_info:
.Lc1b7:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	jne .Lc1b4
.Lc1b5:
	cmpq $1,7(%r14)
	jne .Lc1b4
.Lc1bn:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc1b4:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmV1_info, .-Bench_zdmV1_info
.section .data
.align 8
.align 1
.globl Bench_zdmV1_closure
.type Bench_zdmV1_closure, @object
Bench_zdmV1_closure:
	.quad	Bench_zdmV1_info
.section .text
.align 8
.align 8
	.quad	12884901911
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmV2_info
.type Bench_zdmV2_info, @function
Bench_zdmV2_info:
.Lc1bC:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	jne .Lc1bz
.Lc1bA:
	cmpq $2,7(%r14)
	jne .Lc1bz
.Lc1bS:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc1bz:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmV2_info, .-Bench_zdmV2_info
.section .data
.align 8
.align 1
.globl Bench_zdmV2_closure
.type Bench_zdmV2_closure, @object
Bench_zdmV2_closure:
	.quad	Bench_zdmV2_info
.section .text
.align 8
.align 8
	.quad	12884901911
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmV3_info
.type Bench_zdmV3_info, @function
Bench_zdmV3_info:
.Lc1c7:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	jne .Lc1c4
.Lc1c5:
	cmpq $3,7(%r14)
	jne .Lc1c4
.Lc1cn:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc1c4:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmV3_info, .-Bench_zdmV3_info
.section .data
.align 8
.align 1
.globl Bench_zdmV3_closure
.type Bench_zdmV3_closure, @object
Bench_zdmV3_closure:
	.quad	Bench_zdmV3_info
.section .text
.align 8
.align 8
	.quad	12884901911
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmV4_info
.type Bench_zdmV4_info, @function
Bench_zdmV4_info:
.Lc1cC:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	jne .Lc1cz
.Lc1cA:
	cmpq $4,7(%r14)
	jne .Lc1cz
.Lc1cS:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc1cz:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmV4_info, .-Bench_zdmV4_info
.section .data
.align 8
.align 1
.globl Bench_zdmV4_closure
.type Bench_zdmV4_closure, @object
Bench_zdmV4_closure:
	.quad	Bench_zdmV4_info
.section .text
.align 8
.align 8
	.quad	12884901911
	.quad	0
	.long	14
	.long	0
.globl Bench_zdmV5_info
.type Bench_zdmV5_info, @function
Bench_zdmV5_info:
.Lc1d7:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	jne .Lc1d4
.Lc1d5:
	cmpq $5,7(%r14)
	jne .Lc1d4
.Lc1dn:
	movq %rsi,%rbx
	jmp stg_ap_v_fast
.Lc1d4:
	movq %rdi,%rbx
	jmp stg_ap_v_fast
	.size Bench_zdmV5_info, .-Bench_zdmV5_info
.section .data
.align 8
.align 1
.globl Bench_zdmV5_closure
.type Bench_zdmV5_closure, @object
Bench_zdmV5_closure:
	.quad	Bench_zdmV5_info
.section .data
.align 8
.align 1
.globl Bench_zdtrModule3_closure
.type Bench_zdtrModule3_closure, @object
Bench_zdtrModule3_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtrModule4_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtrModule1_closure
.type Bench_zdtrModule1_closure, @object
Bench_zdtrModule1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtrModule2_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtrModule_closure
.type Bench_zdtrModule_closure, @object
Bench_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	Bench_zdtrModule3_closure+1
	.quad	Bench_zdtrModule1_closure+1
	.quad	3
.section .data
.align 8
.align 1
.Lr12d_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	ghczmprim_GHCziTypes_zdtcIntzh_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtcUBool1_closure
.type Bench_zdtcUBool1_closure, @object
Bench_zdtcUBool1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTYPE_con_info
	.quad	ghczmprim_GHCziTypes_IntRep_closure+5
	.quad	3
.section .data
.align 8
.align 1
.Lr12e_closure:
	.quad	ghczmprim_GHCziTypes_BoxedRep_con_info
	.quad	ghczmprim_GHCziTypes_Unlifted_closure+2
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtcEnv1_closure
.type Bench_zdtcEnv1_closure, @object
Bench_zdtcEnv1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTYPE_con_info
	.quad	.Lr12e_closure+4
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtcTm1_closure
.type Bench_zdtcTm1_closure, @object
Bench_zdtcTm1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtcTm2_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtcTm_closure
.type Bench_zdtcTm_closure, @object
Bench_zdtcTm_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtcTm1_closure+1
	.quad	Bench_zdtcEnv1_closure+5
	.quad	-2878453073501510836
	.quad	-4733423519339279694
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.Lr12f_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	Bench_zdtcTm_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqLam1_closure
.type Bench_zdtczqLam1_closure, @object
Bench_zdtczqLam1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12f_closure+1
	.quad	.Lr12f_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqLam2_closure
.type Bench_zdtczqLam2_closure, @object
Bench_zdtczqLam2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqLam3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqLam_closure
.type Bench_zdtczqLam_closure, @object
Bench_zdtczqLam_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqLam2_closure+1
	.quad	Bench_zdtczqLam1_closure+4
	.quad	-3524063601800429326
	.quad	-4493398555150110212
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqApp1_closure
.type Bench_zdtczqApp1_closure, @object
Bench_zdtczqApp1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12f_closure+1
	.quad	Bench_zdtczqLam1_closure+4
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqApp2_closure
.type Bench_zdtczqApp2_closure, @object
Bench_zdtczqApp2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqApp3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqApp_closure
.type Bench_zdtczqApp_closure, @object
Bench_zdtczqApp_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqApp2_closure+1
	.quad	Bench_zdtczqApp1_closure+4
	.quad	-5662301625060840726
	.quad	-9169093959386975264
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVar1_closure
.type Bench_zdtczqVar1_closure, @object
Bench_zdtczqVar1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12d_closure+1
	.quad	.Lr12f_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVar2_closure
.type Bench_zdtczqVar2_closure, @object
Bench_zdtczqVar2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqVar3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqVar_closure
.type Bench_zdtczqVar_closure, @object
Bench_zdtczqVar_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqVar2_closure+1
	.quad	Bench_zdtczqVar1_closure+4
	.quad	4508178726301664457
	.quad	-1453759458814780869
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtcEnv2_closure
.type Bench_zdtcEnv2_closure, @object
Bench_zdtcEnv2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtcEnv3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtcEnv_closure
.type Bench_zdtcEnv_closure, @object
Bench_zdtcEnv_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtcEnv2_closure+1
	.quad	Bench_zdtcEnv1_closure+5
	.quad	7061011450812421339
	.quad	1788705321429966803
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqNil1_closure
.type Bench_zdtczqNil1_closure, @object
Bench_zdtczqNil1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	Bench_zdtcEnv_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqNil2_closure
.type Bench_zdtczqNil2_closure, @object
Bench_zdtczqNil2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqNil3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqNil_closure
.type Bench_zdtczqNil_closure, @object
Bench_zdtczqNil_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqNil2_closure+1
	.quad	Bench_zdtczqNil1_closure+1
	.quad	-264239873706794582
	.quad	-3313360495854654565
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtcVal1_closure
.type Bench_zdtcVal1_closure, @object
Bench_zdtcVal1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtcVal2_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtcVal_closure
.type Bench_zdtcVal_closure, @object
Bench_zdtcVal_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtcVal1_closure+1
	.quad	Bench_zdtcEnv1_closure+5
	.quad	8101832447291787660
	.quad	-7774064956836739313
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.Lr12g_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	Bench_zdtcVal_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.Lr12h_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12g_closure+1
	.quad	.Lr12g_closure+1
	.quad	3
.section .data
.align 8
.align 1
.Lr12i_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12g_closure+1
	.quad	Bench_zdtczqNil1_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqCons1_closure
.type Bench_zdtczqCons1_closure, @object
Bench_zdtczqCons1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	Bench_zdtczqNil1_closure+1
	.quad	.Lr12i_closure+4
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqCons2_closure
.type Bench_zdtczqCons2_closure, @object
Bench_zdtczqCons2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqCons3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqCons_closure
.type Bench_zdtczqCons_closure, @object
Bench_zdtczqCons_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqCons2_closure+1
	.quad	Bench_zdtczqCons1_closure+4
	.quad	-8696425934646881608
	.quad	-1396997218882901478
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.Lr12j_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12f_closure+1
	.quad	.Lr12g_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVLam1_closure
.type Bench_zdtczqVLam1_closure, @object
Bench_zdtczqVLam1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	Bench_zdtczqNil1_closure+1
	.quad	.Lr12j_closure+4
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVLam2_closure
.type Bench_zdtczqVLam2_closure, @object
Bench_zdtczqVLam2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqVLam3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqVLam_closure
.type Bench_zdtczqVLam_closure, @object
Bench_zdtczqVLam_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqVLam2_closure+1
	.quad	Bench_zdtczqVLam1_closure+4
	.quad	8575279210019183731
	.quad	-6971945200731464941
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVApp1_closure
.type Bench_zdtczqVApp1_closure, @object
Bench_zdtczqVApp1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12g_closure+1
	.quad	.Lr12h_closure+4
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVApp2_closure
.type Bench_zdtczqVApp2_closure, @object
Bench_zdtczqVApp2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqVApp3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqVApp_closure
.type Bench_zdtczqVApp_closure, @object
Bench_zdtczqVApp_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqVApp2_closure+1
	.quad	Bench_zdtczqVApp1_closure+4
	.quad	1772331824952503489
	.quad	9145789107541800646
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVVar1_closure
.type Bench_zdtczqVVar1_closure, @object
Bench_zdtczqVVar1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12d_closure+1
	.quad	.Lr12g_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqVVar2_closure
.type Bench_zdtczqVVar2_closure, @object
Bench_zdtczqVVar2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqVVar3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqVVar_closure
.type Bench_zdtczqVVar_closure, @object
Bench_zdtczqVVar_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqVVar2_closure+1
	.quad	Bench_zdtczqVVar1_closure+4
	.quad	-5921534054878791943
	.quad	-5905458342394945971
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtcUBool2_closure
.type Bench_zdtcUBool2_closure, @object
Bench_zdtcUBool2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtcUBool3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtcUBool_closure
.type Bench_zdtcUBool_closure, @object
Bench_zdtcUBool_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtcUBool2_closure+1
	.quad	Bench_zdtcUBool1_closure+5
	.quad	-7562799265918050835
	.quad	6804316153566187693
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.Lr12k_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	Bench_zdtcUBool_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqUBool1_closure
.type Bench_zdtczqUBool1_closure, @object
Bench_zdtczqUBool1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	.Lr12d_closure+1
	.quad	.Lr12k_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl Bench_zdtczqUBool2_closure
.type Bench_zdtczqUBool2_closure, @object
Bench_zdtczqUBool2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Bench_zdtczqUBool3_bytes
.section .data
.align 8
.align 1
.globl Bench_zdtczqUBool_closure
.type Bench_zdtczqUBool_closure, @object
Bench_zdtczqUBool_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	Bench_zdtrModule_closure+1
	.quad	Bench_zdtczqUBool2_closure+1
	.quad	Bench_zdtczqUBool1_closure+4
	.quad	-2396717353190459166
	.quad	1432359671528842540
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
.Lr12m_closure:
	.quad	stg_unpack_cstring_info
	.quad	0
	.quad	0
	.quad	0
	.quad	.Lr12l_bytes
.section .data
.align 8
.align 1
.Lr12n_closure:
	.quad	stg_unpack_cstring_info
	.quad	0
	.quad	0
	.quad	0
	.quad	Bench_zdtrModule4_bytes
.section .data
.align 8
.align 1
.Lr12o_closure:
	.quad	stg_unpack_cstring_info
	.quad	0
	.quad	0
	.quad	0
	.quad	Bench_zdtrModule2_bytes
.section .data
.align 8
.align 1
.Lr12q_closure:
	.quad	stg_unpack_cstring_info
	.quad	0
	.quad	0
	.quad	0
	.quad	.Lr12p_bytes
.section .data
.align 8
.align 1
.Lr12u_closure:
	.quad	base_GHCziStackziTypes_SrcLoc_con_info
	.quad	.Lr12n_closure
	.quad	.Lr12o_closure
	.quad	.Lr12q_closure
	.quad	stg_INTLIKE_closure+785
	.quad	stg_INTLIKE_closure+577
	.quad	stg_INTLIKE_closure+785
	.quad	stg_INTLIKE_closure+721
	.quad	0
.section .data
.align 8
.align 1
.Lr12v_closure:
	.quad	base_GHCziStackziTypes_PushCallStack_con_info
	.quad	.Lr12m_closure
	.quad	.Lr12u_closure+1
	.quad	base_GHCziStackziTypes_EmptyCallStack_closure+1
	.quad	0
.section .data
.align 8
.align 1
.Lu1eu_srt:
	.quad	stg_SRT_2_info
	.quad	base_GHCziErr_undefined_closure
	.quad	.Lr12v_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu1eu_srt-(.Lr12w_info)+0
.Lr12w_info:
.Lc1er:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1es
.Lc1et:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1eq
.Lc1ep:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	leaq .Lr12v_closure+2(%rip),%r14
	addq $-16,%rbp
	jmp base_GHCziErr_undefined_info
.Lc1eq:
	jmp *(%rbx)
.Lc1es:
	jmp *-16(%r13)
	.size .Lr12w_info, .-.Lr12w_info
.section .data
.align 8
.align 1
.Lr12w_closure:
	.quad	.Lr12w_info
	.quad	0
	.quad	0
	.quad	0
.section .text
.align 8
.align 8
	.quad	8589934606
	.quad	0
	.long	14
	.long	.Lr12w_closure-(Bench_var_info)+0
.globl Bench_var_info
.type Bench_var_info, @function
Bench_var_info:
.Lc1eK:
	jmp .Lc1eC
.Lc1eZ:
	decq %rsi
	movq 6(%r14),%r14
.Lc1eC:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	je .Lc1eH
.Lc1eI:
	testq %rsi,%rsi
	jne .Lc1eZ
.Lc1f0:
	movq 14(%r14),%rbx
	jmp *(%rbp)
.Lc1eH:
	leaq .Lr12w_closure(%rip),%rbx
	jmp *(%rbx)
	.size Bench_var_info, .-Bench_var_info
.section .data
.align 8
.align 1
.globl Bench_var_closure
.type Bench_var_closure, @object
Bench_var_closure:
	.quad	Bench_var_info
	.quad	0
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	Bench_var_closure-(Bench_eval_info)+0
.globl Bench_eval_info
.type Bench_eval_info, @function
Bench_eval_info:
.Lc1fi:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1fj
.Lc1f9:
	movq %rsi,%rax
	andl $7,%esi
	cmpq $3,%rsi
	jae .Lc1g4
.Lu1gb:
	cmpq $2,%rsi
	jb .Lc1fe
.Lc1ff:
	movq $.Lc1fu_info,-24(%rbp)
	movq 6(%rax),%rsi
	movq %r14,%rbx
	movq %rbx,-16(%rbp)
	movq 14(%rax),%rax
	movq %rax,-8(%rbp)
	addq $-24,%rbp
	jmp Bench_eval_info
.Lc1fe:
	movq 7(%rax),%rsi
	jmp Bench_var_info
.align 8
	.quad	1
	.long	30
	.long	0
.Lc1g3_info:
.Lc1g3:
	movq 8(%rbp),%r14
	addq $16,%rbp
.Ln1gp:
	movq %rbx,%rax
.Lc1g4:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1g9
.Lc1g8:
	movq 5(%rax),%rax
	movq $Bench_VLam_con_info,-16(%r12)
	movq %r14,-8(%r12)
	movq %rax,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
.Lc1g9:
	movq $24,904(%r13)
	movq $.Lc1g3_info,-16(%rbp)
	movq %rax,%rbx
	movq %r14,-8(%rbp)
	addq $-16,%rbp
	jmp stg_gc_unpt_r1
.Lc1fj:
	leaq Bench_eval_closure(%rip),%rbx
	jmp *-8(%r13)
.Lc1fU:
	movq $24,904(%r13)
	jmp stg_gc_unpt_r1
.align 8
	.quad	2
	.long	30
	.long	Bench_eval_closure-(.Lc1fu_info)+0
.Lc1fu_info:
.Lc1fu:
	movq 8(%rbp),%r14
	movq 16(%rbp),%rax
	movq %rbx,%rcx
	andl $7,%ecx
	cmpq $2,%rcx
	jne .Lc1fG
.Lc1fO:
	movq $.Lc1fM_info,(%rbp)
	movq %rax,%rsi
	movq 14(%rbx),%rax
	movq %rax,8(%rbp)
	movq 6(%rbx),%rax
	movq %rax,16(%rbp)
	jmp Bench_eval_info
.Lc1fG:
	movq $.Lc1fz_info,8(%rbp)
	movq %rax,%rsi
	movq %rbx,16(%rbp)
	addq $8,%rbp
	jmp Bench_eval_info
.align 8
	.quad	1
	.long	30
	.long	0
.Lc1fz_info:
.Lc1fz:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1fJ
.Lc1fI:
	movq $Bench_VApp_con_info,-16(%r12)
	movq 8(%rbp),%rax
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	leaq -13(%r12),%rbx
	addq $16,%rbp
	jmp *(%rbp)
.Lc1fJ:
	movq $24,904(%r13)
	jmp stg_gc_unpt_r1
.align 8
	.quad	2
	.long	30
	.long	Bench_eval_closure-(.Lc1fM_info)+0
.Lc1fM_info:
.Lc1fM:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1fU
.Lc1fT:
	movq $Bench_Cons_con_info,-16(%r12)
	movq 16(%rbp),%rax
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	movq 8(%rbp),%rsi
	addq $24,%rbp
	leaq -14(%r12),%r14
	jmp .Lc1f9
	.size Bench_eval_info, .-Bench_eval_info
.section .data
.align 8
.align 1
.globl Bench_eval_closure
.type Bench_eval_closure, @object
Bench_eval_closure:
	.quad	Bench_eval_info
	.quad	0
.section .text
.align 8
.align 8
	.quad	12884901907
	.quad	0
	.long	14
	.long	Bench_eval_closure-(Bench_conv_info)+0
.globl Bench_conv_info
.type Bench_conv_info, @function
Bench_conv_info:
.Lc1gD:
	leaq -48(%rbp),%rax
	cmpq %r15,%rax
	jae .Lc1gs
.Lc1gE:
	leaq Bench_conv_closure(%rip),%rbx
	jmp *-8(%r13)
.align 8
	.quad	68
	.long	30
	.long	Bench_conv_closure-(.Lc1iJ_info)+0
.Lc1iJ_info:
.Lc1iJ:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1j4
.Lc1j3:
	movq $Bench_Cons_con_info,-16(%r12)
	movq 16(%rbp),%rax
	movq %rax,-8(%r12)
	movq 32(%rbp),%rax
	movq %rax,(%r12)
	movq $.Lc1iT_info,(%rbp)
	movq 24(%rbp),%rsi
	leaq -14(%r12),%r14
	movq %rbx,32(%rbp)
	jmp Bench_eval_info
.Lc1j4:
	movq $24,904(%r13)
	jmp stg_gc_unpt_r1
.align 8
	.quad	68
	.long	30
	.long	Bench_conv_closure-(.Lc1h7_info)+0
.Lc1h7_info:
.Lc1h7:
	movq 8(%rbp),%r14
	movq 16(%rbp),%rax
	movq 24(%rbp),%rcx
	movq 32(%rbp),%rdx
	addq $40,%rbp
	jmp .Lc1h8
.align 8
	.quad	69
	.long	30
	.long	Bench_conv_closure-(.Lc1iA_info)+0
.Lc1iA_info:
.Lc1iA:
	movq 8(%rbp),%r14
	movq 16(%rbp),%rax
	movq 24(%rbp),%rcx
	movq 32(%rbp),%rdx
	movq 40(%rbp),%rsi
	addq $48,%rbp
	jmp .Lc1iB
.align 8
	.quad	67
	.long	30
	.long	Bench_conv_closure-(.Lc1iu_info)+0
.Lc1iu_info:
.Lc1iu:
	movq 8(%rbp),%r14
	movq 16(%rbp),%rax
	movq 24(%rbp),%rcx
	addq $32,%rbp
.Ln1kj:
	movq %rcx,72(%rsp)
	movq %rbx,%rcx
	movq 72(%rsp),%rbx
	jmp .Lc1iv
.align 8
	.quad	68
	.long	30
	.long	Bench_conv_closure-(.Lc1hA_info)+0
.Lc1hA_info:
.Lc1hA:
	movq 8(%rbp),%r14
	movq 16(%rbp),%rsi
	movq 24(%rbp),%rax
	movq 32(%rbp),%rcx
	addq $40,%rbp
	jmp .Lc1hB
.align 8
	.quad	66
	.long	30
	.long	Bench_conv_closure-(.Lc1hg_info)+0
.Lc1hg_info:
.Lc1hg:
	movq 16(%rbp),%rdi
	movq 8(%rbp),%rax
	leaq 1(%rax),%r14
	addq $24,%rbp
.Ln1kg:
	movq %rbx,%rsi
	jmp .Lc1gs
.align 8
	.quad	452
	.long	30
	.long	Bench_conv_closure-(.Lc1iT_info)+0
.Lc1iT_info:
.Lc1iT:
	movq 32(%rbp),%rdi
	movq 8(%rbp),%rax
	leaq 1(%rax),%r14
	addq $40,%rbp
.Ln1ke:
	movq %rbx,%rsi
	jmp .Lc1gs
.align 8
	.quad	66
	.long	30
	.long	Bench_conv_closure-(.Lc1hJ_info)+0
.Lc1hJ_info:
.Lc1hJ:
	movq 16(%rbp),%rsi
	movq 8(%rbp),%rax
	leaq 1(%rax),%r14
	addq $24,%rbp
.Ln1kf:
	movq %rbx,%rdi
	jmp .Lc1gs
.align 8
	.quad	67
	.long	30
	.long	Bench_conv_closure-(.Lc1jr_info)+0
.Lc1jr_info:
.Lc1jr:
	movq 8(%rbp),%r14
	movq 24(%rbp),%rdi
	movq 16(%rbp),%rsi
	addq $32,%rbp
.Lc1gs:
	movq %rsi,%rax
	andl $7,%eax
	cmpq $3,%rax
	jae .Lc1gB
.Lu1jG:
	cmpq $2,%rax
	jb .Lc1gz
.Lc1gA:
	movq 6(%rsi),%rax
	movq 14(%rsi),%rbx
	movq %rdi,%rcx
	movq %rdi,%rdx
	andl $7,%edx
	cmpq $2,%rdx
	jne .Ls16u
.Lc1iv:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1iX
.Lc1iW:
	movq 6(%rcx),%rdx
	movq 14(%rcx),%rsi
	movq $Bench_VVar_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rcx
.Ln1kk:
	movq %rbx,72(%rsp)
	movq %rcx,%rbx
	movq 72(%rsp),%rcx
.Lc1iB:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1j0
.Lc1iZ:
	movq $Bench_Cons_con_info,-16(%r12)
	movq %rdx,-8(%r12)
	movq %rbx,(%r12)
	movq $.Lc1iJ_info,-40(%rbp)
	movq %r14,%rdx
	leaq -14(%r12),%r14
	movq %rdx,-32(%rbp)
	movq %rax,-24(%rbp)
	movq %rcx,-16(%rbp)
	movq %rbx,-8(%rbp)
	addq $-40,%rbp
	jmp Bench_eval_info
.Lc1j0:
	movq $24,904(%r13)
	movq $.Lc1iA_info,-48(%rbp)
	movq %r14,-40(%rbp)
	movq %rax,-32(%rbp)
	movq %rcx,-24(%rbp)
	movq %rdx,-16(%rbp)
	movq %rsi,-8(%rbp)
	addq $-48,%rbp
	jmp stg_gc_unpt_r1
.Lc1gB:
	movq %rdi,%rax
	andl $7,%eax
	cmpq $3,%rax
	jne .Ls16u
.Lc1js:
	movq $.Lc1jr_info,-32(%rbp)
	movq %rdi,%rax
	movq 5(%rdi),%rdi
	movq %rsi,%rbx
	movq 5(%rsi),%rsi
	movq %r14,%rcx
	movq %rcx,-24(%rbp)
	movq 13(%rbx),%rbx
	movq %rbx,-16(%rbp)
	movq 13(%rax),%rax
	movq %rax,-8(%rbp)
	addq $-32,%rbp
	jmp Bench_conv_info
.Lc1gz:
	movq %rdi,%rax
	andl $7,%eax
	cmpq $1,%rax
	je .Lc1ie
.Ls16u:
	movq %rdi,%rax
	andl $7,%edi
	cmpq $2,%rdi
	je .Lc1hv
.Lc1gO:
	movq %rsi,%rbx
	andl $7,%esi
	cmpq $2,%rsi
	je .Lc1h2
.Lc1gW:
	xorl %ebx,%ebx
	jmp *(%rbp)
.align 8
	.quad	66
	.long	30
	.long	Bench_conv_closure-(.Lc1h1_info)+0
.Lc1h1_info:
.Lc1h1:
	movq 8(%rbp),%r14
	movq 16(%rbp),%rax
	addq $24,%rbp
.Lc1h2:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1hk
.Lc1hj:
	movq 6(%rbx),%rcx
	movq 14(%rbx),%rdx
	movq $Bench_VVar_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rbx
.Lc1h8:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1hn
.Lc1hm:
	movq $Bench_Cons_con_info,-16(%r12)
	movq %rcx,-8(%r12)
	movq %rbx,(%r12)
	movq $.Lc1hg_info,-24(%rbp)
	movq %rdx,%rsi
	movq %r14,%rbx
	leaq -14(%r12),%r14
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	addq $-24,%rbp
	jmp Bench_eval_info
.Lc1hn:
	movq $24,904(%r13)
	movq $.Lc1h7_info,-40(%rbp)
	movq %r14,-32(%rbp)
	movq %rax,-24(%rbp)
	movq %rcx,-16(%rbp)
	movq %rdx,-8(%rbp)
	addq $-40,%rbp
	jmp stg_gc_unpt_r1
.align 8
	.quad	66
	.long	30
	.long	Bench_conv_closure-(.Lc1hu_info)+0
.Lc1hu_info:
.Lc1hu:
	movq 8(%rbp),%r14
	movq 16(%rbp),%rsi
	addq $24,%rbp
.Ln1kh:
	movq %rbx,%rax
.Lc1hv:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1hN
.Lc1hM:
	movq 6(%rax),%rbx
	movq 14(%rax),%rcx
	movq $Bench_VVar_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rax
.Ln1ki:
	movq %rbx,64(%rsp)
	movq %rax,%rbx
	movq 64(%rsp),%rax
.Lc1hB:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1hQ
.Lc1hP:
	movq $Bench_Cons_con_info,-16(%r12)
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	movq $.Lc1hJ_info,-24(%rbp)
	movq %rsi,%rax
	movq %rcx,%rsi
	movq %r14,%rbx
	leaq -14(%r12),%r14
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	addq $-24,%rbp
	jmp Bench_eval_info
.Lc1hQ:
	movq $24,904(%r13)
	movq $.Lc1hA_info,-40(%rbp)
	movq %r14,-32(%rbp)
	movq %rsi,-24(%rbp)
	movq %rax,-16(%rbp)
	movq %rcx,-8(%rbp)
	addq $-40,%rbp
	jmp stg_gc_unpt_r1
.Lc1ie:
	movq 7(%rsi),%rax
	cmpq 7(%rdi),%rax
	sete %al
	movzbl %al,%ebx
	jmp *(%rbp)
.Lc1hN:
	movq $16,904(%r13)
	movq $.Lc1hu_info,-24(%rbp)
	movq %rax,%rbx
	movq %r14,-16(%rbp)
	movq %rsi,-8(%rbp)
	addq $-24,%rbp
	jmp stg_gc_unpt_r1
.Lc1iX:
	movq $16,904(%r13)
	movq $.Lc1iu_info,-32(%rbp)
	movq %rbx,%rdx
	movq %rcx,%rbx
	movq %r14,-24(%rbp)
	movq %rax,-16(%rbp)
	movq %rdx,-8(%rbp)
	addq $-32,%rbp
	jmp stg_gc_unpt_r1
.Lc1hk:
	movq $16,904(%r13)
	movq $.Lc1h1_info,-24(%rbp)
	movq %r14,-16(%rbp)
	movq %rax,-8(%rbp)
	addq $-24,%rbp
	jmp stg_gc_unpt_r1
	.size Bench_conv_info, .-Bench_conv_info
.section .data
.align 8
.align 1
.globl Bench_conv_closure
.type Bench_conv_closure, @object
Bench_conv_closure:
	.quad	Bench_conv_info
	.quad	0
.section .text
.align 8
.align 8
	.quad	4294967300
	.quad	0
	.long	14
	.long	0
.globl Bench_VVar_info
.type Bench_VVar_info, @function
Bench_VVar_info:
.Lc1kr:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1kv
.Lc1ku:
	movq $Bench_VVar_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc1kv:
	movq $16,904(%r13)
	leaq Bench_VVar_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_VVar_info, .-Bench_VVar_info
.section .data
.align 8
.align 1
.globl Bench_VVar_closure
.type Bench_VVar_closure, @object
Bench_VVar_closure:
	.quad	Bench_VVar_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Bench_VLam_info
.type Bench_VLam_info, @function
Bench_VLam_info:
.Lc1kE:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1kI
.Lc1kH:
	movq $Bench_VLam_con_info,-16(%r12)
	movq %r14,-8(%r12)
	movq %rsi,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
.Lc1kI:
	movq $24,904(%r13)
	leaq Bench_VLam_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_VLam_info, .-Bench_VLam_info
.section .data
.align 8
.align 1
.globl Bench_VLam_closure
.type Bench_VLam_closure, @object
Bench_VLam_closure:
	.quad	Bench_VLam_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Bench_VApp_info
.type Bench_VApp_info, @function
Bench_VApp_info:
.Lc1kR:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1kV
.Lc1kU:
	movq $Bench_VApp_con_info,-16(%r12)
	movq %r14,-8(%r12)
	movq %rsi,(%r12)
	leaq -13(%r12),%rbx
	jmp *(%rbp)
.Lc1kV:
	movq $24,904(%r13)
	leaq Bench_VApp_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_VApp_info, .-Bench_VApp_info
.section .data
.align 8
.align 1
.globl Bench_VApp_closure
.type Bench_VApp_closure, @object
Bench_VApp_closure:
	.quad	Bench_VApp_info
.section .data
.align 8
.align 1
.globl Bench_Nil_closure
.type Bench_Nil_closure, @object
Bench_Nil_closure:
	.quad	Bench_Nil_con_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Bench_Cons_info
.type Bench_Cons_info, @function
Bench_Cons_info:
.Lc1l5:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1l9
.Lc1l8:
	movq $Bench_Cons_con_info,-16(%r12)
	movq %r14,-8(%r12)
	movq %rsi,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
.Lc1l9:
	movq $24,904(%r13)
	leaq Bench_Cons_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_Cons_info, .-Bench_Cons_info
.section .data
.align 8
.align 1
.globl Bench_Cons_closure
.type Bench_Cons_closure, @object
Bench_Cons_closure:
	.quad	Bench_Cons_info
.section .text
.align 8
.align 8
	.quad	4294967300
	.quad	0
	.long	14
	.long	0
.globl Bench_Var_info
.type Bench_Var_info, @function
Bench_Var_info:
.Lc1li:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1lm
.Lc1ll:
	movq $Bench_Var_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc1lm:
	movq $16,904(%r13)
	leaq Bench_Var_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_Var_info, .-Bench_Var_info
.section .data
.align 8
.align 1
.globl Bench_Var_closure
.type Bench_Var_closure, @object
Bench_Var_closure:
	.quad	Bench_Var_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.long	14
	.long	0
.globl Bench_App_info
.type Bench_App_info, @function
Bench_App_info:
.Lc1lv:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .Lc1lz
.Lc1ly:
	movq $Bench_App_con_info,-16(%r12)
	movq %r14,-8(%r12)
	movq %rsi,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
.Lc1lz:
	movq $24,904(%r13)
	leaq Bench_App_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_App_info, .-Bench_App_info
.section .data
.align 8
.align 1
.globl Bench_App_closure
.type Bench_App_closure, @object
Bench_App_closure:
	.quad	Bench_App_info
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
.globl Bench_Lam_info
.type Bench_Lam_info, @function
Bench_Lam_info:
.Lc1lI:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1lM
.Lc1lL:
	movq $Bench_Lam_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -5(%r12),%rbx
	jmp *(%rbp)
.Lc1lM:
	movq $16,904(%r13)
	leaq Bench_Lam_closure(%rip),%rbx
	jmp *-8(%r13)
	.size Bench_Lam_info, .-Bench_Lam_info
.section .data
.align 8
.align 1
.globl Bench_Lam_closure
.type Bench_Lam_closure, @object
Bench_Lam_closure:
	.quad	Bench_Lam_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1lR_str:
	.string "main:Bench.VVar"
.section .text
.align 8
.align 8
	.quad	i1lR_str-(Bench_VVar_con_info)+0
	.quad	4294967296
	.long	3
	.long	0
.globl Bench_VVar_con_info
.type Bench_VVar_con_info, @object
Bench_VVar_con_info:
.Lc1lQ:
	incq %rbx
	jmp *(%rbp)
	.size Bench_VVar_con_info, .-Bench_VVar_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1lW_str:
	.string "main:Bench.VLam"
.section .text
.align 8
.align 8
	.quad	i1lW_str-(Bench_VLam_con_info)+0
	.quad	2
	.long	4
	.long	1
.globl Bench_VLam_con_info
.type Bench_VLam_con_info, @object
Bench_VLam_con_info:
.Lc1lV:
	addq $2,%rbx
	jmp *(%rbp)
	.size Bench_VLam_con_info, .-Bench_VLam_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1m1_str:
	.string "main:Bench.VApp"
.section .text
.align 8
.align 8
	.quad	i1m1_str-(Bench_VApp_con_info)+0
	.quad	2
	.long	4
	.long	2
.globl Bench_VApp_con_info
.type Bench_VApp_con_info, @object
Bench_VApp_con_info:
.Lc1m0:
	addq $3,%rbx
	jmp *(%rbp)
	.size Bench_VApp_con_info, .-Bench_VApp_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1m6_str:
	.string "main:Bench.Nil"
.section .text
.align 8
.align 8
	.quad	i1m6_str-(Bench_Nil_con_info)+0
	.quad	4294967296
	.long	3
	.long	0
.globl Bench_Nil_con_info
.type Bench_Nil_con_info, @object
Bench_Nil_con_info:
.Lc1m5:
	incq %rbx
	jmp *(%rbp)
	.size Bench_Nil_con_info, .-Bench_Nil_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1mb_str:
	.string "main:Bench.Cons"
.section .text
.align 8
.align 8
	.quad	i1mb_str-(Bench_Cons_con_info)+0
	.quad	2
	.long	4
	.long	1
.globl Bench_Cons_con_info
.type Bench_Cons_con_info, @object
Bench_Cons_con_info:
.Lc1ma:
	addq $2,%rbx
	jmp *(%rbp)
	.size Bench_Cons_con_info, .-Bench_Cons_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1mg_str:
	.string "main:Bench.Var"
.section .text
.align 8
.align 8
	.quad	i1mg_str-(Bench_Var_con_info)+0
	.quad	4294967296
	.long	3
	.long	0
.globl Bench_Var_con_info
.type Bench_Var_con_info, @object
Bench_Var_con_info:
.Lc1mf:
	incq %rbx
	jmp *(%rbp)
	.size Bench_Var_con_info, .-Bench_Var_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1ml_str:
	.string "main:Bench.App"
.section .text
.align 8
.align 8
	.quad	i1ml_str-(Bench_App_con_info)+0
	.quad	2
	.long	4
	.long	1
.globl Bench_App_con_info
.type Bench_App_con_info, @object
Bench_App_con_info:
.Lc1mk:
	addq $2,%rbx
	jmp *(%rbp)
	.size Bench_App_con_info, .-Bench_App_con_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1mq_str:
	.string "main:Bench.Lam"
.section .text
.align 8
.align 8
	.quad	i1mq_str-(Bench_Lam_con_info)+0
	.quad	1
	.long	2
	.long	2
.globl Bench_Lam_con_info
.type Bench_Lam_con_info, @object
Bench_Lam_con_info:
.Lc1mp:
	addq $3,%rbx
	jmp *(%rbp)
	.size Bench_Lam_con_info, .-Bench_Lam_con_info
.section .note.GNU-stack,"",@progbits
.ident "GHC 9.6.2"
