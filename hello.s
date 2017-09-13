_start:
    movq $len, %rdx
    movq $msg, %rcx
    movq $1,   %rbx
    movq $4,   %rax
    int  $0x80

    movq $0,   %rbx
    movq $1,   %rax
    int        $0x80


.data
msg:
    .ascii    "Hello, world!\n"   # our dear string
    len = . - msg                 # length of our dear string
