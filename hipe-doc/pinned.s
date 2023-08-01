	.file	"pinned.c"
	.text
	.p2align 4
	.globl	foo
	.type	foo, @function
foo:
.LFB4:
	endbr64
	testb	$1, %dil
	jne	.L2
	movq	(%rdi), %rax
	leaq	8(%r12), %rdx
	addq	$10, %rax
	movq	%rax, (%r12)
	movq	%r12, %rax
	movq	%rdx, %r12
	ret
.L2:
	pushq	%r14
	pushq	%r13
	pushq	%rbp
	leaq	-1(%rdi), %rbp
	pushq	%rbx
	subq	$24, %rsp
	movq	-1(%rdi), %rax
	testb	$1, %al
	jne	.L4
	movq	(%rax), %rax
	leaq	8(%r12), %r14
	movq	%r12, %rbx
	addq	$10, %rax
	movq	%rax, (%r12)
	movq	8(%rbp), %rax
	movq	%r14, %r12
	testb	$1, %al
	jne	.L10
.L20:
	movq	(%rax), %rax
	movq	%r14, 8(%rsp)
	addq	$10, %rax
	movq	%rax, (%r14)
	leaq	8(%r14), %rax
.L11:
	movq	%rbx, %xmm0
	leaq	16(%rax), %rdx
	movhps	8(%rsp), %xmm0
	movq	%rdx, %r12
	movups	%xmm0, (%rax)
	addq	$24, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	orq	$1, %rax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r13
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L4:
	.cfi_restore_state
	movq	-1(%rax), %rdx
	leaq	-1(%rax), %rbx
	testb	$1, %dl
	jne	.L6
	movq	(%rdx), %rdx
	movq	%r12, %r14
	leaq	8(%r12), %r12
	addq	$10, %rdx
	movq	%rdx, -8(%r12)
.L7:
	movq	8(%rbx), %rax
	testb	$1, %al
	jne	.L8
	movq	(%rax), %rax
	movq	%r12, %rdx
	leaq	8(%r12), %r12
	movq	%rdx, 8(%rsp)
	addq	$10, %rax
	movq	%rax, -8(%r12)
.L9:
	movq	%r14, %xmm0
	movq	%r12, %rbx
	leaq	16(%r12), %r14
	movhps	8(%rsp), %xmm0
	orq	$1, %rbx
	movups	%xmm0, (%r12)
	movq	8(%rbp), %rax
	movq	%r14, %r12
	testb	$1, %al
	je	.L20
.L10:
	leaq	-1(%rax), %rbp
	movq	-1(%rax), %rax
	testb	$1, %al
	jne	.L12
	movq	(%rax), %rax
	leaq	8(%r14), %r12
	addq	$10, %rax
	movq	%rax, (%r14)
.L13:
	movq	8(%rbp), %rax
	testb	$1, %al
	jne	.L14
	movq	(%rax), %rax
	movq	%r12, %rdx
	leaq	8(%r12), %r12
	movq	%rdx, 8(%rsp)
	addq	$10, %rax
	movq	%rax, -8(%r12)
.L15:
	movq	%r14, %xmm0
	movq	%r12, %rcx
	leaq	16(%r12), %rax
	movhps	8(%rsp), %xmm0
	orq	$1, %rcx
	movq	%rcx, 8(%rsp)
	movups	%xmm0, (%r12)
	jmp	.L11
	.p2align 4,,10
	.p2align 3
.L6:
	movq	-1(%rdx), %rdi
	leaq	-1(%rdx), %r14
	call	foo
	movq	8(%r14), %rdi
	movq	%rax, %r13
	call	foo
	movq	%r13, %xmm0
	movq	%rax, %xmm2
	movq	%r12, %r14
	leaq	16(%r12), %r12
	punpcklqdq	%xmm2, %xmm0
	orq	$1, %r14
	movups	%xmm0, -16(%r12)
	jmp	.L7
	.p2align 4,,10
	.p2align 3
.L14:
	movq	-1(%rax), %rdi
	leaq	-1(%rax), %r13
	call	foo
	movq	8(%r13), %rdi
	movq	%rax, %rbp
	call	foo
	movq	%rbp, %xmm1
	movq	%rax, %r8
	movq	%r12, %rax
	leaq	16(%r12), %r12
	movq	%r8, %xmm5
	orq	$1, %rax
	punpcklqdq	%xmm5, %xmm1
	movq	%rax, 8(%rsp)
	movups	%xmm1, -16(%r12)
	jmp	.L15
	.p2align 4,,10
	.p2align 3
.L12:
	movq	-1(%rax), %rdi
	leaq	-1(%rax), %r14
	call	foo
	movq	8(%r14), %rdi
	movq	%rax, %r13
	call	foo
	movq	%r13, %xmm0
	movq	%rax, %xmm4
	movq	%r12, %r14
	leaq	16(%r12), %r12
	punpcklqdq	%xmm4, %xmm0
	orq	$1, %r14
	movups	%xmm0, -16(%r12)
	jmp	.L13
	.p2align 4,,10
	.p2align 3
.L8:
	movq	-1(%rax), %rdi
	leaq	-1(%rax), %r13
	call	foo
	movq	8(%r13), %rdi
	movq	%rax, %rbx
	call	foo
	movq	%rbx, %xmm1
	movq	%rax, %r8
	movq	%r12, %rax
	leaq	16(%r12), %r12
	movq	%r8, %xmm3
	orq	$1, %rax
	punpcklqdq	%xmm3, %xmm1
	movq	%rax, 8(%rsp)
	movups	%xmm1, -16(%r12)
	jmp	.L9
	.cfi_endproc
.LFE4:
	.size	foo, .-foo
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB5:
	.cfi_startproc
	endbr64
	xorl	%eax, %eax
	ret
	.cfi_endproc
.LFE5:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 11.3.0-1ubuntu1~22.04.1) 11.3.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
