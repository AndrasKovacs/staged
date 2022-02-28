	.file	"rts.c"
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"TODO: perform GC"
	.text
	.globl	perform_gc
	.type	perform_gc, @function
perform_gc:
	endbr64
	pushq	%rax
	popq	%rax
	subq	$8, %rsp
	leaq	.LC0(%rip), %rdi
	call	*puts@GOTPCREL(%rip)
	movl	$0, %edi
	call	*exit@GOTPCREL(%rip)
	.size	perform_gc, .-perform_gc
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC1:
	.string	"handle_gc_signal caught a signal which is not SIGSEGV\n"
	.align 8
.LC2:
	.string	"segfault occurred at address: %p\n"
	.align 8
.LC3:
	.string	"handled gc signal, frame pointer is: %p\n"
	.text
	.globl	handle_gc_signal
	.type	handle_gc_signal, @function
handle_gc_signal:
	endbr64
	pushq	%rbx
	cmpl	$11, %edi
	jne	.L8
	movq	hp_end(%rip), %rcx
	cmpq	%r12, %rcx
	ja	.L5
	movq	page_size(%rip), %rdi
	leaq	(%rcx,%rdi,8), %rcx
	cmpq	%rcx, %r12
	jb	.L6
.L5:
	movq	16(%rsi), %rcx
	leaq	.LC2(%rip), %rdx
	movl	$1, %esi
	movq	stderr(%rip), %rdi
	movl	$0, %eax
	call	*__fprintf_chk@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
.L8:
	movq	stderr(%rip), %rcx
	movl	$54, %edx
	movl	$1, %esi
	leaq	.LC1(%rip), %rdi
	call	*fwrite@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
.L6:
	movq	104(%rdx), %rbx
	movq	%rbx, %rdx
	leaq	.LC3(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	%rbx, %rdi
	call	perform_gc
	.size	handle_gc_signal, .-handle_gc_signal
	.section	.rodata.str1.1
.LC4:
	.string	"DEBUG: initializing RTS"
	.section	.rodata.str1.8
	.align 8
.LC5:
	.string	"failed to get system memory size"
	.section	.rodata.str1.1
.LC6:
	.string	"DEBUG: available RAM: %ld\n"
.LC7:
	.string	"failed to set stack limit"
.LC8:
	.string	"failed to install GC handler\n"
.LC9:
	.string	"DEBUG: GC handler installed"
.LC10:
	.string	"failed to get page size\n"
.LC11:
	.string	"DEBUG: page size: %ld\n"
	.section	.rodata.str1.8
	.align 8
.LC12:
	.string	"DEBUG: initial heap size: %ld\n"
	.section	.rodata.str1.1
.LC13:
	.string	"failed to allocate memory\n"
	.section	.rodata.str1.8
	.align 8
.LC14:
	.string	"failed to set guard page protection"
	.text
	.globl	init_rts
	.type	init_rts, @function
init_rts:
	endbr64
	pushq	%r13
	pushq	%rbp
	pushq	%rbx
	addq	$-128, %rsp
	leaq	.LC4(%rip), %rdi
	call	*puts@GOTPCREL(%rip)
	leaq	16(%rsp), %rdi
	call	*sysinfo@GOTPCREL(%rip)
	cmpl	$-1, %eax
	je	.L17
	movq	48(%rsp), %rdx
	leaq	.LC6(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	48(%rsp), %rax
	shrq	%rax
	movq	%rax, (%rsp)
	movq	%rax, 8(%rsp)
	movq	%rsp, %rsi
	movl	$3, %edi
	call	*setrlimit@GOTPCREL(%rip)
	cmpl	$-1, %eax
	je	.L18
	movl	$4, 136+gc_sigaction(%rip)
	leaq	handle_gc_signal(%rip), %rax
	movq	%rax, gc_sigaction(%rip)
	movl	$0, %edx
	leaq	gc_sigaction(%rip), %rsi
	movl	$11, %edi
	call	*sigaction@GOTPCREL(%rip)
	cmpl	$-1, %eax
	je	.L19
	leaq	.LC9(%rip), %rdi
	call	*puts@GOTPCREL(%rip)
	movl	$30, %edi
	call	*sysconf@GOTPCREL(%rip)
	movq	%rax, %rdx
	movq	%rax, page_size(%rip)
	cmpq	$-1, %rax
	je	.L20
	leaq	.LC11(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movl	$67108864, %edx
	leaq	.LC12(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	page_size(%rip), %rbp
	leaq	67108864(%rbp), %rsi
	movq	%rbp, %rdi
	call	*aligned_alloc@GOTPCREL(%rip)
	movq	%rax, %rbx
	testq	%rax, %rax
	je	.L21
	leaq	67108864(%rax), %r13
	movl	$0, %edx
	movq	%rbp, %rsi
	movq	%r13, %rdi
	call	*mprotect@GOTPCREL(%rip)
	cmpl	$-1, %eax
	je	.L22
	movq	%rbx, hp_start(%rip)
	movq	%r13, hp_end(%rip)
	movq	%rbx, %r12
	subq	$-128, %rsp
	popq	%rbx
	popq	%rbp
	popq	%r13
	ret
.L17:
	movq	stderr(%rip), %rcx
	movl	$32, %edx
	movl	$1, %esi
	leaq	.LC5(%rip), %rdi
	call	*fwrite@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
.L18:
	movq	stderr(%rip), %rcx
	movl	$25, %edx
	movl	$1, %esi
	leaq	.LC7(%rip), %rdi
	call	*fwrite@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
.L19:
	movq	stderr(%rip), %rcx
	movl	$29, %edx
	movl	$1, %esi
	leaq	.LC8(%rip), %rdi
	call	*fwrite@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
.L20:
	movq	stderr(%rip), %rcx
	movl	$24, %edx
	movl	$1, %esi
	leaq	.LC10(%rip), %rdi
	call	*fwrite@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
.L21:
	movq	stderr(%rip), %rcx
	movl	$26, %edx
	movl	$1, %esi
	leaq	.LC13(%rip), %rdi
	call	*fwrite@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
.L22:
	movq	stderr(%rip), %rcx
	movl	$35, %edx
	movl	$1, %esi
	leaq	.LC14(%rip), %rdi
	call	*fwrite@GOTPCREL(%rip)
	movl	$1, %edi
	call	*exit@GOTPCREL(%rip)
	.size	init_rts, .-init_rts
	.section	.rodata.str1.1
.LC15:
	.string	"\nRTS info"
.LC16:
	.string	"bytes allocated: %ld\n"
.LC17:
	.string	"number of gc runs: %ld\n"
.LC18:
	.string	"start of heap: %p\n"
.LC19:
	.string	"end of heap: %p\n"
.LC20:
	.string	"size of current heap: %ld\n"
.LC21:
	.string	"page size: %ld\n\n"
	.text
	.globl	trace
	.type	trace, @function
trace:
	endbr64
	subq	$8, %rsp
	leaq	.LC15(%rip), %rdi
	call	*puts@GOTPCREL(%rip)
	movq	bytes_allocated(%rip), %rdx
	leaq	.LC16(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	num_gc_runs(%rip), %rdx
	leaq	.LC17(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	hp_start(%rip), %rdx
	leaq	.LC18(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	hp_end(%rip), %rdx
	leaq	.LC19(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	hp_end(%rip), %rdx
	subq	hp_start(%rip), %rdx
	sarq	$3, %rdx
	leaq	.LC20(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movq	page_size(%rip), %rdx
	leaq	.LC21(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	addq	$8, %rsp
	jmp	*__printf_chk@GOTPCREL(%rip)
	.size	trace, .-trace
	.section	.rodata.str1.1
.LC22:
	.string	"test_stack_limit: %ld\n"
	.text
	.globl	test_stack_limit
	.type	test_stack_limit, @function
test_stack_limit:
	endbr64
	movl	$0, %edx
	leaq	.LC22(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	jmp	*__printf_chk@GOTPCREL(%rip)
	.size	test_stack_limit, .-test_stack_limit
	.section	.rodata.str1.1
.LC23:
	.string	"test_gc start"
.LC24:
	.string	"test_gc end"
	.text
	.globl	test_gc
	.type	test_gc, @function
test_gc:
	endbr64
	subq	$8, %rsp
	movq	hp_end(%rip), %r12
	leaq	.LC23(%rip), %rdi
	call	*puts@GOTPCREL(%rip)
	movl	$0, %edi
#APP
# 271 "rts.c" 1
	leaq 10(%r12), %r12; movq %r12, (%r12)
# 0 "" 2
#NO_APP
	leaq	.LC24(%rip), %rdi
	addq	$8, %rsp
	jmp	*puts@GOTPCREL(%rip)
	.size	test_gc, .-test_gc
	.globl	cons
	.type	cons, @function
cons:
	endbr64
#APP
# 283 "rts.c" 1
	leaq 16(%r12), %r12; movq %r12, (%r12)
# 0 "" 2
#NO_APP
	movq	%rsi, -16(%r12)
	movq	%rdx, -8(%r12)
	leaq	-16(%r12), %rax
	salq	$16, %rax
	orq	$8321, %rax
	ret
	.size	cons, .-cons
	.globl	mapsuc
	.type	mapsuc, @function
mapsuc:
	endbr64
	testb	$127, %sil
	jne	.L30
	movl	$0, %eax
	ret
.L30:
	pushq	%rbp
	pushq	%rbx
	subq	$8, %rsp
	movq	%rdi, %rbp
	sarq	$16, %rsi
	movq	%rsi, %rbx
	andq	$-8, %rbx
	movq	8(%rbx), %rsi
	call	mapsuc
	movq	(%rbx), %rdx
	addq	$1, %rdx
	movq	%rbp, %rdi
#APP
# 283 "rts.c" 1
	leaq 16(%r12), %r12; movq %r12, (%r12)
# 0 "" 2
#NO_APP
	movq	%rdx, -16(%r12)
	movq	%rax, -8(%r12)
	leaq	-16(%r12), %rax
	salq	$16, %rax
	orq	$8321, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	ret
	.size	mapsuc, .-mapsuc
	.globl	reverse_go
	.type	reverse_go, @function
reverse_go:
	endbr64
	movq	%rdx, %rax
	testb	$127, %sil
	jne	.L42
	ret
.L42:
	subq	$40, %rsp
	movq	%rdi, %rcx
	sarq	$16, %rsi
	andq	$-8, %rsi
	movq	%rdi, (%rsp)
	movq	$1, 8(%rsp)
	movq	(%rsi), %rdx
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movq	%rsp, %rdi
#APP
# 283 "rts.c" 1
	leaq 16(%r12), %r12; movq %r12, (%r12)
# 0 "" 2
#NO_APP
	movq	%rdx, -16(%r12)
	movq	%rax, -8(%r12)
	leaq	-16(%r12), %rdx
	salq	$16, %rdx
	orq	$8321, %rdx
	movq	16(%rsp), %rsi
	movq	%rcx, %rdi
	call	reverse_go
	addq	$40, %rsp
	ret
	.size	reverse_go, .-reverse_go
	.section	.rodata.str1.1
.LC25:
	.string	"%ld\n"
	.text
	.globl	main
	.type	main, @function
main:
	endbr64
	subq	$8, %rsp
	movl	$2, %edx
	leaq	.LC25(%rip), %rsi
	movl	$1, %edi
	movl	$0, %eax
	call	*__printf_chk@GOTPCREL(%rip)
	movl	$0, %eax
	addq	$8, %rsp
	ret
	.size	main, .-main
	.globl	page_size
	.bss
	.align 8
	.type	page_size, @object
	.size	page_size, 8
page_size:
	.zero	8
	.globl	gc_sigaction
	.align 32
	.type	gc_sigaction, @object
	.size	gc_sigaction, 152
gc_sigaction:
	.zero	152
	.globl	hp_end
	.align 8
	.type	hp_end, @object
	.size	hp_end, 8
hp_end:
	.zero	8
	.globl	hp_start
	.align 8
	.type	hp_start, @object
	.size	hp_start, 8
hp_start:
	.zero	8
	.globl	num_gc_runs
	.align 8
	.type	num_gc_runs, @object
	.size	num_gc_runs, 8
num_gc_runs:
	.zero	8
	.globl	bytes_allocated
	.align 8
	.type	bytes_allocated, @object
	.size	bytes_allocated, 8
bytes_allocated:
	.zero	8
	.ident	"GCC: (Ubuntu 10.2.0-5ubuntu1~20.04) 10.2.0"
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
