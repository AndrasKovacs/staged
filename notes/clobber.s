	.file	"clobber.c"
	.text
	.p2align 4
	.globl	foo
	.type	foo, @function
foo:
	endbr64
	pushq	%rbp
	xorl	%eax, %eax
	pushq	%rbx
	movq	%rdi, %rbx
	subq	$8, %rsp
	movq	(%rdi), %rbp
	call	unknown2@PLT
	movq	(%rbx), %rax
	addq	$8, %rsp
	popq	%rbx
	addq	%rbp, %rax
	popq	%rbp
	ret
	.size	foo, .-foo
	.ident	"GCC: (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	 1f - 0f
	.long	 4f - 1f
	.long	 5
0:
	.string	 "GNU"
1:
	.align 8
	.long	 0xc0000002
	.long	 3f - 2f
2:
	.long	 0x3
3:
	.align 8
4:
