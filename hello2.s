_start:
    movq $14 , %rdi
    movq $0x00600129, %r8
    call prn
    call exit
    ret
exit:
    movq $0,   %rbx
    movq $1,   %rax
    int        $0x80
    ret
prn:
    movq %rdi, %rdx
    movq %r8 , %rcx
    movq $1,   %rbx
    movq $4,   %rax
    int  $0x80
    ret
.data
msg:
    .ascii    "Hello, world!\n"   # our dear string
