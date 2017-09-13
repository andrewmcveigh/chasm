_start:
    push %rdi
    movq %rdi, %rdx
    movq %r8,  %rcx
    movq $1,   %rbx
    movq $4,   %rax
    int $0x80
    pop %rdi
    ret
