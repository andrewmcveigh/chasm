_start:
    movq $0xffffffffffffffff, %rdi
    jmp  p__to_str_int64

p__to_str_int64:
    movq  %rdi , %rax
    movq  $10  , %rbx

loop1:
    movq  $0   , %rdx # clear rdx
    divq  %rbx        # div rax by rbx (10)
    addq  $0x30, %rdx # convert rem in rdx to ascii
    pushq %rdx        # push rdx onto stack
    incq  %rcx        # increment counter
    testq %rax , %rax # if rax not zero?
    jnz   loop1       # recur, else continue

    movq  %rcx , %rdi # put counter in rdi, so we have the length
    movq  $0   , %rax # set rax to 0
    movq  $res , %r15 # address of res in r15

pop_str:
    popq  %rdx
    movq  %rdx , (%r15)
    decq  %rcx
    incq  %r15
    testq %rcx , %rcx
    jnz   pop_str

prn:
    movq  %rdi , %rdx
    movq  $res , %rcx
    movq  $1   , %rbx
    movq  $4   , %rax
    int   $0x80
exit:
    movq  $0   , %rbx
    movq  $1   , %rax
    int   $0x80

.data
res:
    .quad 0
