	.file	"inline_asm.c"
	.text
	.p2align 4
	.globl	kek
	.type	kek, @function
kek:
.LFB27:
	.cfi_startproc
	movq	%rsi, (%rdi)
	xorl	%eax, %eax
	ret
	.cfi_endproc
.LFE27:
	.size	kek, .-kek
	.p2align 4
	.globl	foo
	.type	foo, @function
foo:
.LFB28:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	xorl	%ebx, %ebx
#APP
# 17 "inline_asm.c" 1
	addq %rbx, %rcx; addq %rbx, %rcx
# 0 "" 2
#NO_APP
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE28:
	.size	foo, .-foo
	.p2align 4
	.globl	bar
	.type	bar, @function
bar:
.LFB29:
	.cfi_startproc
	leaq	100(%rdi), %rax
	ret
	.cfi_endproc
.LFE29:
	.size	bar, .-bar
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%ld\n"
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB30:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	xorl	%ebx, %ebx
#APP
# 17 "inline_asm.c" 1
	addq %rbx, %rcx; addq %rbx, %rcx
# 0 "" 2
#NO_APP
	xorl	%edx, %edx
	movl	$.LC0, %esi
	movl	$1, %edi
	xorl	%eax, %eax
	call	__printf_chk
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE30:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.3.0-21ubuntu1~16.04) 9.3.0"
	.section	.note.GNU-stack,"",@progbits
