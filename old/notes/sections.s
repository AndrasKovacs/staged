	.file	"sections.c"
	.intel_syntax noprefix
	.text
	.globl	c1
	.data
	.type	c1, @object
	.size	c1, 1
c1:
	.byte	97
	.text
	.globl	f1
	.type	f1, @function
f1:
.LFB23:
	.cfi_startproc
	lea	eax, [rdi+200]
	ret
	.cfi_endproc
.LFE23:
	.size	f1, .-f1
	.globl	c2
	.data
	.type	c2, @object
	.size	c2, 1
c2:
	.byte	98
	.globl	c3
	.type	c3, @object
	.size	c3, 1
c3:
	.byte	99
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%c\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB24:
	.cfi_startproc
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	movsx	edx, BYTE PTR c1[rip]
	mov	esi, OFFSET FLAT:.LC0
	mov	edi, 1
	mov	eax, 0
	call	__printf_chk
	mov	eax, 0
	add	rsp, 8
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE24:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.3.0-10ubuntu2~16.04) 9.3.0"
	.section	.note.GNU-stack,"",@progbits
