_start:
    movq    $0x4d2,%rdi
    pushq  %rbp
    callq  p__to_str_int64
    #popq   %r8
    callq  prn
    callq  exit
    retq
exit:
    movq    $0x0,%rbx
    movq    $0x1,%rax
    int    $0x80
    retq
prn:
    movq    %rdi,%rdx
    movq    %r8,%rcx
    movq    $0x1,%rbx
    movq    $0x4,%rax
    int    $0x80
    retq
p__to_str_int64:
    movq    %rdi,%rax
    movq    $0xa,%rbx
convert:
    movq   $0x0,%rdx
    divq   %rbx
    addq   $0x30,%rdx
    pushq  %rdx
    incq   %rcx
    testq  %rax,%rax
    jne    convert
    pushq  %rcx
    movq   $12,  %rax # syscall 12 (brk) -> rax
    xorq   %rdi, %rdi # pass 0 as 1st arg, meaning: return current heap offset
    syscall           # call kernel: current top of the bss section will be
                      # returned in rax
    movq   %rax, %rbp # save ptr in rbp
    addq   %rcx, %rax # add str len to heap pointer
    movq   %rax, %rdi # put it into rdi
    movq   $12, %rax  # syscall 12 (brk) alloc (count) new bytes
    syscall           # call kernel
    popq   %rcx       # put length back in rcx
    movq   %rcx, %rdi # put length in rdi
    movq   %rbp, %r8  # and save ptr in r8 for prn
pop_str:
    popq   %rdx
    movq   %rdx, (%rbp)
    decq   %rcx
    incq   %rbp
    test   %rcx, %rcx
    jne    pop_str
    retq

.data
res:
    .quad 0

.bss
m:
    .quad 0
