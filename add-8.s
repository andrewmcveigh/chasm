	.file	"add-8.c"
	.section	.text.unlikely,"ax",@progbits
.LCOLDB0:
	.text
.LHOTB0:
	.p2align 4,,15
	.globl	add_int
	.type	add_int, @function
add_int:
.LFB0:
	.cfi_startproc
	leal	(%rdi,%rsi), %eax
	ret
	.cfi_endproc
.LFE0:
	.size	add_int, .-add_int
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.text.unlikely
.LCOLDB1:
	.section	.text.startup,"ax",@progbits
.LHOTB1:
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$3, %esi
	movl	$1, %edi
	call	add_int@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	movsbl	%al, %eax
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.section	.text.unlikely
.LCOLDE1:
	.section	.text.startup
.LHOTE1:
	.ident	"GCC: (GNU) 5.4.0"
	.section	.note.GNU-stack,"",@progbits
