	.text
	.file	"rtsclang.c"
	.globl	perform_gc                      # -- Begin function perform_gc
	.p2align	4, 0x90
	.type	perform_gc,@function
perform_gc:                             # @perform_gc
# %bb.0:
	pushq	%rax
	movl	$.Lstr, %edi
	callq	puts
	xorl	%edi, %edi
	callq	exit
.Lfunc_end0:
	.size	perform_gc, .Lfunc_end0-perform_gc
                                        # -- End function
	.globl	handle_gc_signal                # -- Begin function handle_gc_signal
	.p2align	4, 0x90
	.type	handle_gc_signal,@function
handle_gc_signal:                       # @handle_gc_signal
# %bb.0:
	pushq	%rax
	cmpl	$11, %edi
	jne	.LBB1_4
# %bb.1:
	movq	hp_end(%rip), %rax
	movq	hp(%rip), %rcx
	cmpq	%rcx, %rax
	ja	.LBB1_5
# %bb.2:
	movq	page_size(%rip), %rdi
	leaq	(%rax,%rdi,8), %rax
	cmpq	%rax, %rcx
	jae	.LBB1_5
# %bb.3:
	movq	104(%rdx), %rsi
	movl	$.L.str.3, %edi
	xorl	%eax, %eax
	callq	printf
	callq	perform_gc
.LBB1_4:
	movq	stderr(%rip), %rcx
	movl	$.L.str.1, %edi
	movl	$54, %esi
	movl	$1, %edx
	callq	fwrite
	movl	$1, %edi
	callq	exit
.LBB1_5:
	movq	stderr(%rip), %rdi
	movq	16(%rsi), %rdx
	movl	$.L.str.2, %esi
	xorl	%eax, %eax
	callq	fprintf
	movl	$1, %edi
	callq	exit
.Lfunc_end1:
	.size	handle_gc_signal, .Lfunc_end1-handle_gc_signal
                                        # -- End function
	.globl	init_rts                        # -- Begin function init_rts
	.p2align	4, 0x90
	.type	init_rts,@function
init_rts:                               # @init_rts
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$128, %rsp
	movl	$.Lstr.26, %edi
	callq	puts
	leaq	16(%rsp), %rdi
	callq	sysinfo
	cmpl	$-1, %eax
	je	.LBB2_1
# %bb.3:
	movq	48(%rsp), %rsi
	movl	$.L.str.6, %edi
	xorl	%eax, %eax
	callq	printf
	movq	48(%rsp), %rax
	shrq	%rax
	movq	%rax, (%rsp)
	movq	%rax, 8(%rsp)
	movq	%rsp, %rsi
	movl	$3, %edi
	callq	setrlimit
	cmpl	$-1, %eax
	je	.LBB2_4
# %bb.5:
	movl	$4, gc_sigaction+136(%rip)
	movq	$handle_gc_signal, gc_sigaction(%rip)
	movl	$gc_sigaction, %esi
	movl	$11, %edi
	xorl	%edx, %edx
	callq	sigaction
	cmpl	$-1, %eax
	je	.LBB2_6
# %bb.7:
	movl	$.Lstr.27, %edi
	callq	puts
	movl	$30, %edi
	callq	sysconf
	movq	%rax, page_size(%rip)
	cmpq	$-1, %rax
	je	.LBB2_8
# %bb.9:
	movl	$.L.str.11, %edi
	movq	%rax, %rsi
	xorl	%eax, %eax
	callq	printf
	movl	$.L.str.12, %edi
	movl	$67108864, %esi                 # imm = 0x4000000
	xorl	%eax, %eax
	callq	printf
	movq	page_size(%rip), %r14
	leaq	67108864(%r14), %rsi
	movq	%r14, %rdi
	callq	aligned_alloc
	testq	%rax, %rax
	je	.LBB2_10
# %bb.11:
	movq	%rax, %rbx
	leaq	67108864(%rax), %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	xorl	%edx, %edx
	callq	mprotect
	cmpl	$-1, %eax
	je	.LBB2_12
# %bb.13:
	movq	%rbx, hp_start(%rip)
	movq	%r15, hp_end(%rip)
	movq	%rbx, hp(%rip)
	addq	$128, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB2_1:
	movq	stderr(%rip), %rcx
	movl	$.L.str.5, %edi
	movl	$32, %esi
	jmp	.LBB2_2
.LBB2_4:
	movq	stderr(%rip), %rcx
	movl	$.L.str.7, %edi
	movl	$25, %esi
	jmp	.LBB2_2
.LBB2_6:
	movq	stderr(%rip), %rcx
	movl	$.L.str.8, %edi
	movl	$29, %esi
	jmp	.LBB2_2
.LBB2_8:
	movq	stderr(%rip), %rcx
	movl	$.L.str.10, %edi
	movl	$24, %esi
	jmp	.LBB2_2
.LBB2_10:
	movq	stderr(%rip), %rcx
	movl	$.L.str.13, %edi
	movl	$26, %esi
	jmp	.LBB2_2
.LBB2_12:
	movq	stderr(%rip), %rcx
	movl	$.L.str.14, %edi
	movl	$35, %esi
.LBB2_2:
	movl	$1, %edx
	callq	fwrite
	movl	$1, %edi
	callq	exit
.Lfunc_end2:
	.size	init_rts, .Lfunc_end2-init_rts
                                        # -- End function
	.globl	trace                           # -- Begin function trace
	.p2align	4, 0x90
	.type	trace,@function
trace:                                  # @trace
# %bb.0:
	pushq	%rax
	movl	$.Lstr.28, %edi
	callq	puts
	movq	bytes_allocated(%rip), %rsi
	movl	$.L.str.16, %edi
	xorl	%eax, %eax
	callq	printf
	movq	num_gc_runs(%rip), %rsi
	movl	$.L.str.17, %edi
	xorl	%eax, %eax
	callq	printf
	movq	hp_start(%rip), %rsi
	movl	$.L.str.18, %edi
	xorl	%eax, %eax
	callq	printf
	movq	hp_end(%rip), %rsi
	movl	$.L.str.19, %edi
	xorl	%eax, %eax
	callq	printf
	movq	hp_end(%rip), %rsi
	subq	hp_start(%rip), %rsi
	sarq	$3, %rsi
	movl	$.L.str.20, %edi
	xorl	%eax, %eax
	callq	printf
	movq	page_size(%rip), %rsi
	movl	$.L.str.21, %edi
	xorl	%eax, %eax
	popq	%rcx
	jmp	printf                          # TAILCALL
.Lfunc_end3:
	.size	trace, .Lfunc_end3-trace
                                        # -- End function
	.globl	test_stack_limit                # -- Begin function test_stack_limit
	.p2align	4, 0x90
	.type	test_stack_limit,@function
test_stack_limit:                       # @test_stack_limit
# %bb.0:
	movl	$.L.str.22, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	jmp	printf                          # TAILCALL
.Lfunc_end4:
	.size	test_stack_limit, .Lfunc_end4-test_stack_limit
                                        # -- End function
	.globl	test_gc                         # -- Begin function test_gc
	.p2align	4, 0x90
	.type	test_gc,@function
test_gc:                                # @test_gc
# %bb.0:
	pushq	%rax
	movq	hp_end(%rip), %rax
	movq	%rax, hp(%rip)
	movl	$.Lstr.29, %edi
	callq	puts
	movq	hp(%rip), %rax
	xorl	%edi, %edi
	#APP
	leaq	10(%rax), %rax
	movq	%rax, (%rax)
	#NO_APP
	movq	%rax, hp(%rip)
	movl	$.Lstr.30, %edi
	popq	%rax
	jmp	puts                            # TAILCALL
.Lfunc_end5:
	.size	test_gc, .Lfunc_end5-test_gc
                                        # -- End function
	.globl	cons                            # -- Begin function cons
	.p2align	4, 0x90
	.type	cons,@function
cons:                                   # @cons
# %bb.0:
	movq	hp(%rip), %rcx
	#APP
	leaq	16(%rcx), %rcx
	movq	%rcx, (%rcx)
	#NO_APP
	movq	%rcx, hp(%rip)
	leaq	-16(%rcx), %rax
	movq	%rsi, -16(%rcx)
	movq	%rdx, -8(%rcx)
	shlq	$16, %rax
	orq	$8321, %rax                     # imm = 0x2081
	retq
.Lfunc_end6:
	.size	cons, .Lfunc_end6-cons
                                        # -- End function
	.globl	mapsuc                          # -- Begin function mapsuc
	.p2align	4, 0x90
	.type	mapsuc,@function
mapsuc:                                 # @mapsuc
# %bb.0:
	pushq	%r14
	pushq	%rbx
	pushq	%rax
	movq	%rsi, %rbx
	testb	$127, %bl
	je	.LBB7_2
# %bb.1:
	movq	%rdi, %r14
	sarq	$16, %rbx
	andq	$-8, %rbx
	movq	8(%rbx), %rsi
	callq	mapsuc
	movq	(%rbx), %rdx
	addq	$1, %rdx
	movq	hp(%rip), %rsi
	movq	%r14, %rdi
	#APP
	leaq	16(%rsi), %rsi
	movq	%rsi, (%rsi)
	#NO_APP
	movq	%rsi, hp(%rip)
	leaq	-16(%rsi), %rcx
	movq	%rdx, -16(%rsi)
	movq	%rax, -8(%rsi)
	shlq	$16, %rcx
	orq	$8321, %rcx                     # imm = 0x2081
	movq	%rcx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB7_2:
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Lfunc_end7:
	.size	mapsuc, .Lfunc_end7-mapsuc
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function reverse_go
.LCPI8_0:
	.long	1                               # 0x1
	.long	0                               # 0x0
	.long	0                               # 0x0
	.long	0                               # 0x0
	.text
	.globl	reverse_go
	.p2align	4, 0x90
	.type	reverse_go,@function
reverse_go:                             # @reverse_go
# %bb.0:
	testb	$127, %sil
	je	.LBB8_1
# %bb.2:
	movq	%rdi, %rax
	sarq	$16, %rsi
	andq	$-8, %rsi
	movq	%rdi, -24(%rsp)
	movaps	.LCPI8_0(%rip), %xmm0           # xmm0 = [1,0,0,0]
	movups	%xmm0, -16(%rsp)
	movq	(%rsi), %r8
	movq	8(%rsi), %rsi
	movq	%rsi, -8(%rsp)
	movq	hp(%rip), %rcx
	leaq	-24(%rsp), %rdi
	#APP
	leaq	16(%rcx), %rcx
	movq	%rcx, (%rcx)
	#NO_APP
	movq	%rcx, hp(%rip)
	leaq	-16(%rcx), %rdx
	movq	%r8, -16(%rcx)
	movq	%rsi, -8(%rcx)
	shlq	$16, %rdx
	orq	$8321, %rdx                     # imm = 0x2081
	movq	-8(%rsp), %rsi
	movq	%rax, %rdi
	jmp	reverse_go                      # TAILCALL
.LBB8_1:
	movq	%rdx, %rax
	retq
.Lfunc_end8:
	.size	reverse_go, .Lfunc_end8-reverse_go
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
# %bb.0:
	pushq	%rax
	movl	$.L.str.25, %edi
	movl	$2, %esi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end9:
	.size	main, .Lfunc_end9-main
                                        # -- End function
	.type	bytes_allocated,@object         # @bytes_allocated
	.bss
	.globl	bytes_allocated
	.p2align	3
bytes_allocated:
	.quad	0                               # 0x0
	.size	bytes_allocated, 8

	.type	num_gc_runs,@object             # @num_gc_runs
	.globl	num_gc_runs
	.p2align	3
num_gc_runs:
	.quad	0                               # 0x0
	.size	num_gc_runs, 8

	.type	gc_sigaction,@object            # @gc_sigaction
	.globl	gc_sigaction
	.p2align	3
gc_sigaction:
	.zero	152
	.size	gc_sigaction, 152

	.type	.L.str.1,@object                # @.str.1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.1:
	.asciz	"handle_gc_signal caught a signal which is not SIGSEGV\n"
	.size	.L.str.1, 55

	.type	hp_end,@object                  # @hp_end
	.bss
	.globl	hp_end
	.p2align	3
hp_end:
	.quad	0
	.size	hp_end, 8

	.type	hp,@object                      # @hp
	.globl	hp
	.p2align	3
hp:
	.quad	0
	.size	hp, 8

	.type	page_size,@object               # @page_size
	.globl	page_size
	.p2align	3
page_size:
	.quad	0                               # 0x0
	.size	page_size, 8

	.type	.L.str.2,@object                # @.str.2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.2:
	.asciz	"segfault occurred at address: %p\n"
	.size	.L.str.2, 34

	.type	.L.str.3,@object                # @.str.3
.L.str.3:
	.asciz	"handled gc signal, frame pointer is: %p\n"
	.size	.L.str.3, 41

	.type	.L.str.5,@object                # @.str.5
.L.str.5:
	.asciz	"failed to get system memory size"
	.size	.L.str.5, 33

	.type	.L.str.6,@object                # @.str.6
.L.str.6:
	.asciz	"DEBUG: available RAM: %ld\n"
	.size	.L.str.6, 27

	.type	.L.str.7,@object                # @.str.7
.L.str.7:
	.asciz	"failed to set stack limit"
	.size	.L.str.7, 26

	.type	.L.str.8,@object                # @.str.8
.L.str.8:
	.asciz	"failed to install GC handler\n"
	.size	.L.str.8, 30

	.type	.L.str.10,@object               # @.str.10
.L.str.10:
	.asciz	"failed to get page size\n"
	.size	.L.str.10, 25

	.type	.L.str.11,@object               # @.str.11
.L.str.11:
	.asciz	"DEBUG: page size: %ld\n"
	.size	.L.str.11, 23

	.type	.L.str.12,@object               # @.str.12
.L.str.12:
	.asciz	"DEBUG: initial heap size: %ld\n"
	.size	.L.str.12, 31

	.type	.L.str.13,@object               # @.str.13
.L.str.13:
	.asciz	"failed to allocate memory\n"
	.size	.L.str.13, 27

	.type	.L.str.14,@object               # @.str.14
.L.str.14:
	.asciz	"failed to set guard page protection"
	.size	.L.str.14, 36

	.type	hp_start,@object                # @hp_start
	.bss
	.globl	hp_start
	.p2align	3
hp_start:
	.quad	0
	.size	hp_start, 8

	.type	.L.str.16,@object               # @.str.16
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str.16:
	.asciz	"bytes allocated: %ld\n"
	.size	.L.str.16, 22

	.type	.L.str.17,@object               # @.str.17
.L.str.17:
	.asciz	"number of gc runs: %ld\n"
	.size	.L.str.17, 24

	.type	.L.str.18,@object               # @.str.18
.L.str.18:
	.asciz	"start of heap: %p\n"
	.size	.L.str.18, 19

	.type	.L.str.19,@object               # @.str.19
.L.str.19:
	.asciz	"end of heap: %p\n"
	.size	.L.str.19, 17

	.type	.L.str.20,@object               # @.str.20
.L.str.20:
	.asciz	"size of current heap: %ld\n"
	.size	.L.str.20, 27

	.type	.L.str.21,@object               # @.str.21
.L.str.21:
	.asciz	"page size: %ld\n\n"
	.size	.L.str.21, 17

	.type	.L.str.22,@object               # @.str.22
.L.str.22:
	.asciz	"test_stack_limit: %ld\n"
	.size	.L.str.22, 23

	.type	.L.str.25,@object               # @.str.25
.L.str.25:
	.asciz	"%ld\n"
	.size	.L.str.25, 5

	.type	.Lstr,@object                   # @str
.Lstr:
	.asciz	"TODO: perform GC"
	.size	.Lstr, 17

	.type	.Lstr.26,@object                # @str.26
.Lstr.26:
	.asciz	"DEBUG: initializing RTS"
	.size	.Lstr.26, 24

	.type	.Lstr.27,@object                # @str.27
.Lstr.27:
	.asciz	"DEBUG: GC handler installed"
	.size	.Lstr.27, 28

	.type	.Lstr.28,@object                # @str.28
.Lstr.28:
	.asciz	"\nRTS info"
	.size	.Lstr.28, 10

	.type	.Lstr.29,@object                # @str.29
.Lstr.29:
	.asciz	"test_gc start"
	.size	.Lstr.29, 14

	.type	.Lstr.30,@object                # @str.30
.Lstr.30:
	.asciz	"test_gc end"
	.size	.Lstr.30, 12

	.ident	"Ubuntu clang version 11.0.0-2~ubuntu20.04.1"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym handle_gc_signal
	.addrsig_sym gc_sigaction
