	.file	"gccnotes.c"
	.text
	.globl	test
	.type	test, @function
test:
.LFB66:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	leaq	15(%rsp), %rax
	andq	$-16, %rax
	movq	%r13, (%rax)
	movq	$2, 8(%rax)
	movq	%rax, %r13
	movq	$0, 16(%rax)
	movq	$0, 24(%rax)
	movl	$20, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE66:
	.size	test, .-test
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%d\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB67:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$0, %r13d
	movl	$20, %edx
	movl	$.LC0, %esi
	movl	$1, %edi
	movl	$0, %eax
	call	__printf_chk
	movl	$0, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE67:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.3.0-10ubuntu2~16.04) 9.3.0"
	.section	.note.GNU-stack,"",@progbits
