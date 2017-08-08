.text          # section declaration
               # we must export the entry point to the ELF linker or
.global _start # loader. They conventionally recognize _start as their
               # entry point. Use ld -e foo to override the default.

.equ BUFFEREND, 1
.lcomm buffer, BUFFEREND

_start:
    addq    %rax,%rax
    addq    %rax,%rdi
    addq    %rax,%rcx
    addq    %rax,%rdx
    addq    %rcx,%rax
    addq    %rcx,%rcx
    addq    %rdi,%rax
    addq    %rdi,%rdi

    addq    %rax,%r8
    addq    %rax,%r15
    addq    %rcx,%r8
    addq    %rcx,%r15
    addq    %rdx,%r8
    addq    %rdx,%r15

    addq    %r8,%rax
    addq    %r8,%rcx
    addq    %r8,%rdx
    addq    %r8,%rdi
    addq    %r8,%r8
    addq    %r8,%r9
    addq    %r8,%r15
    addq    %r8,%r8
    addq    %r8,%r15
    addq    %r9,%r8
    addq    %r9,%r15
    addq    %r15,%rax
    addq    %r15,%r8
    addq    %r15,%r15
    ret
.data                 # section declaration

msg:
    .ascii    "Hello, world!\n"   # our dear string
    len = . - msg                 # length of our dear string
