(ns chasm.asm)

(def b8-registers)

(def b16-registers)

(def b32-registers
  [:%eax :%ebx :%ecx]
  )

(def b64-registers
  "https://www3.nd.edu/~dthain/courses/cse40243/fall2015/intel-intro.html
  X86-64 has sixteen (almost) general purpose 64-bit integer registers"
  [:%rax :%rbx :%rcx :%rdx :%rsi :%rdi :%rbp :%rsp
   :%r8  :%r9  :%r10 :%r11 :%r12 :%r13 :%r14 :%r15])
