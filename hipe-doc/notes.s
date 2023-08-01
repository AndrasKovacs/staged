	.file	"notes.c"
	.text
	.p2align 4
	.globl	foo
	.type	foo, @function
foo:
.LFB4:
	endbr64
	testb	$1, %sil
	jne	.L2
	movq	(%rsi), %rax
	leaq	8(%rdi), %rdx
	addq	$10, %rax
	movq	%rax, (%rdi)
	movq	%rdi, %rax
	ret
.L2:
	pushq	%rbp
	leaq	-1(%rsi), %rbp
	pushq	%rbx
	subq	$8, %rsp
	movq	-1(%rsi), %rsi
	call	foo
	movq	8(%rbp), %rsi
	movq	%rdx, %rdi
	movq	%rax, %rbx
	call	foo
	movq	%rax, 8(%rdx)
	movq	%rdx, %rax
	addq	$16, %rdx
	movq	%rbx, -16(%rdx)
	addq	$8, %rsp
	orq	$1, %rax
	popq	%rbx
	popq	%rbp
	ret
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
