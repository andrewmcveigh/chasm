_start:
    mov    $0x2,%rdi
    mov    $0x2,%r8
    callq  p__add_int64
    mov    %rax,%rdi
    callq  p__to_str_int64
    callq  prn
    retq
p__add_int64:
    mov    %rdi,%rax
    add    %r8,%rax
    retq

alloc:
    push   %rcx
    push   %rdi
    mov    $0xc,%rax
    mov    $0x0,%rdi
    syscall
    pop    %rdi
    push   %rax
    add    %rax,%rdi
    mov    $0xc,%rax
    syscall
    pop    %rax
    pop    %rcx
    retq

exit:
    mov    $0x0,%rbx
    mov    $0x1,%rax
    int    $0x80
    retq

prn:
    mov    %rdi,%rdx
    mov    %r8,%rcx
    mov    $0x1,%rbx
    mov    $0x4,%rax
    int    $0x80
    retq

p__to_str_int64:
    mov    %rdi,%rax
    mov    $0xa,%rbx

convert:
    mov    $0x0,%rdx
    div    %rbx
    add    $0x30,%rdx
    push   %rdx
    inc    %rcx
    test   %rax,%rax
    jne    convert
    mov    %rcx,%rdi
    callq  alloc
    mov    %rcx,%rdi
    mov    %rax,%r8
    mov    %rax,%rbx
    mov    $0x0,%rax

pop_str:
    pop    %rdx
    mov    %rdx,(%rbx)
    dec    %rcx
    inc    %rbx
    test   %rcx,%rcx
    jne    pop_str
    retq
