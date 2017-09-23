_start:
#   movq  $0xffffffffffffffff ,  %rax  # 48 c7 c0 ff ff ff ff
#   movq  $1                  ,  %rax  # 48 c7 c0 01 00 00 00
#   movq  $1                  ,  %rdi  # 48 c7 c7 01 00 00 00
#   movq  $1                  ,  %r8   # 49 c7 c0 01 00 00 00
#   movq  $1                  ,  %r15  # 49 c7 c7 01 00 00 00
#   movq  (1)                 ,  %rax  # 48 8b 04 25 01 00 00 00
#   movq  (1)                 ,  %rdi  # 48 8b 3c 25 01 00 00 00
#   movq  (1)                 ,  %r8   # 4c 8b 04 25 01 00 00 00
#   movq  (1)                 ,  %r15  # 4c 8b 3c 25 01 00 00 00
#   movq  %rax                ,  %rax  # 48 89 c0
#   movq  %rax                ,  %rdi  # 48 89 c7
#   movq  %rdi                ,  %rdi  # 48 89 ff
#   movq  %rax                ,  %r8   # 49 89 c0
#   movq  %rax                ,  %r15  # 49 89 c7
#   movq  %rax                ,  %rax  # 48 89 c0
#   movq  %rdi                ,  %rax  # 48 89 f8
#   movq  %r8                 ,  %rax  # 4c 89 c0
#   movq  %r15                ,  %rax  # 4c 89 f8
#   movq  %rax                ,  %rcx  # 48 89 c1
#   movq  %rdi                ,  %rcx  # 48 89 f9
#   movq  %r8                 ,  %rcx  # 4c 89 c1
#   movq  %r15                ,  %rcx  # 4c 89 f9
#   movq  %rax                ,  %rdi  # 48 89 c7
#   movq  %r8                 ,  %rdi  # 4c 89 c7
#   movq  %r15                ,  %rdi  # 4c 89 ff
#   movq  %rax                ,  %r8   # 49 89 c0
#   movq  %rdi                ,  %r8   # 49 89 f8
#   movq  %r8                 ,  %r8   # 4d 89 c0
#   movq  %r15                ,  %r8   # 4d 89 f8
#   movq  %rax                ,  %r15  # 49 89 c7
#   movq  %rdi                ,  %r15  # 49 89 ff
#   movq  %r8                 ,  %r15  # 4d 89 c7
#   movq  %r15                ,  %r15  # 4d 89 ff
#   movq  %rax                ,  (1)   # 48 89 04 25 01 00 00 00
#   movq  %rdi                ,  (1)   # 48 89 3c 25 01 00 00 00
#   movq  %r8                 ,  (1)   # 4c 89 04 25 01 00 00 00
#   movq  %r15                ,  (1)   # 4c 89 3c 25 01 00 00 00
#   movq  (%rax)              ,  %rax  # 48 8b 00
#   movq  (%rax)              ,  %rdi  # 48 8b 38
#   movq  (%rdi)              ,  %rax  # 48 8b 07
#   movq  (%rdi)              ,  %rdi  # 48 8b 3f
#   movq  (%r8)               ,  %rax  # 49 8b 00
#   movq  (%r15)              ,  %rdi  # 49 8b 3f
#   movq  (%rax)              ,  %r8   # 4c 8b 00
#   movq  (%rdi)              ,  %r15  # 4c 8b 3f
#   movq  (%r8)               ,  %r8   # 4d 8b 00
#   movq  (%r15)              ,  %r15  # 4d 8b 3f
#   movq  %rax                , (%rax) # 48 89 00
#   movq  %rax                , (%rdi) # 48 89 07
#   movq  %rdi                , (%rax) # 48 89 38
#   movq  %rdi                , (%rdi) # 48 89 3f
#   movq  %rax                , (%r8)  # 49 89 00
#   movq  %rdi                , (%r15) # 49 89 3f
#   movq  %r8                 , (%rax) # 4c 89 00
#   movq  %r15                , (%rdi) # 4c 89 3f
#   movq  %r8                 , (%r8)  # 4d 89 00
#   movq  %r15                , (%r15) # 4d 89 3f
#   movq  $1                  , (%rax) # 48 c7 00 01 00 00 00
#   movq  $1                  , (%rdi) # 48 c7 07 01 00 00 00
#   movq  $1                  , (%r8)  # 49 c7 00 01 00 00 00
#   movq  $1                  , (%r15) # 49 c7 07 01 00 00 00
#   movq  (%rcx)              , (%rax) # ILLEGAL
#   movq  (1)                 , (%rdi) # ILLEGAL
#   movq  $1                  , (1)    # 48 c7 04 25 01 00 00 00 01 00 00 00
#   movq  (%rax)              , (1)    # ILLEGAL

#   addq  %rax        , %rax   # 48 01 c0
#   addq  %rdi        , %rdi   # 48 01 ff
#   addq  %r8         , %rax   # 4c 01 c0
#   addq  %r8         , %rdi   # 4c 01 c7
#   addq  %r8         , %r8    # 4d 01 c0
#   addq  %r15        , %r15   # 4d 01 ff
#   addq  (%rax)      , %rax   # 48 03 00
#   addq  (%rdi)      , %rdi   # 48 03 3f
#   addq  (%r8)       , %rax   # 49 03 00
#   addq  (%r8)       , %rdi   # 49 03 38
#   addq  (%r8)       , %r8    # 4d 03 00
#   addq  (%r15)      , %r15   # 4d 03 3f
#   addq  $1          , %rax   # 48 83 c0 01
#   addq  $1          , %rdi   # 48 83 c7 01
#   addq  $1          , %r8    # 49 83 c0 01
#   addq  $1          , %r15   # 49 83 c7 01
#   addq  $0x12345678 , %rax   # 48 05 78 56 34 12
#   addq  $0x12345678 , %rdi   # 48 81 c7 78 56 34 12
#   addq  $0x12345678 , %r8    # 49 81 c0 78 56 34 12
#   addq  $0x12345678 , %r15   # 49 81 c7 78 56 34 12
#   addq  (1)         , %rax   # 48 03 04 25 01 00 00 00
#   addq  (1)         , %rdi   # 48 03 3c 25 01 00 00 00
#   addq  (1)         , %r8    # 4c 03 04 25 01 00 00 00
#   addq  (1)         , %r15   # 4c 03 3c 25 01 00 00 00
#   addq  $1          , (1)    # 48 83 04 25 01 00 00 00 01
#   addq  $0x12345678 , (1)    # 48 81 04 25 01 00 00 00 78 56 34 12
#   addq  %rax        , (1)    # 48 01 04 25 01 00 00 00
#   addq  %rdi        , (1)    # 48 01 3c 25 01 00 00 00
#   addq  %r8         , (1)    # 4c 01 04 25 01 00 00 00
#   addq  %r15        , (1)    # 4c 01 3c 25 01 00 00 00
#   addq  %rax        , (%rax) # 48 01 00
#   addq  %rdi        , (%rdi) # 48 01 3f
#   addq  %r8         , (%rax) # 4c 01 00
#   addq  %r15        , (%rdi) # 4c 01 3f
#   addq  %r8         , (%r8)  # 4d 01 00
#   addq  %r15        , (%r15) # 4d 01 3f
#   addq  (%r15)      , (1)    # ILLEGAL

#   subq  %rax        , %rax # 48 29 c0
#   subq  %rdi        , %rdi # 48 29 ff
#   subq  %r8         , %rax # 4c 29 c0
#   subq  %r8         , %rdi # 4c 29 c7
#   subq  %r8         , %r8  # 4d 29 c0
#   subq  %r15        , %r15 # 4d 29 ff
#   subq  $1          , %rax # 48 83 e8 01
#   subq  $1          , %rdi # 48 83 ef 01
#   subq  $1          , %r8  # 49 83 e8 01
#   subq  $1          , %r15 # 49 83 ef 01
#   subq  $0x12345678 , %rax # 48 2d 78 56 34 12
#   subq  $0x12345678 , %rdi # 48 81 ef 78 56 34 12
#   subq  $0x12345678 , %r8  # 49 81 e8 78 56 34 12
#   subq  $0x12345678 , %r15 # 49 81 ef 78 56 34 12
#   subq  (1)         , %rax # 48 2b 04 25 01 00 00 00
#   subq  (1)         , %rdi # 48 2b 3c 25 01 00 00 00
#   subq  (1)         , %r8  # 4c 2b 04 25 01 00 00 00
#   subq  (1)         , %r15 # 4c 2b 3c 25 01 00 00 00
#   subq  $1          , (1)  # 48 83 2c 25 01 00 00 00 01
#   subq  $0x12345678 , (1)  # 48 81 2c 25 01 00 00 00 78 56 34 12
#   subq  (%rax)      , %rax # 48 2b 00
#   subq  (%rdi)      , %rdi # 48 2b 3f
#   subq  (%r8)       , %rax # 49 2b 00
#   subq  (%r8)       , %rdi # 49 2b 38
#   subq  (%r8)       , %r8  # 4d 2b 00
#   subq  (%r15)      , %r15 # 4d 2b 3f

#   testq %rax        , $1     # ILLEGAL
#   testq (%rax)      , $1     # ILLEGAL
#   testq (1)         , $1     # ILLEGAL
#   testq $1          , $1     # ILLEGAL
#   testq %rax        , %rax   # 48 85 c0
#   testq %rdi        , %rdi   # 48 85 ff
#   testq %r8         , %r8    # 4d 85 c0
#   testq %r15        , %r15   # 4d 85 ff
#   testq (%rax)      , %rax   # 48 85 00
#   testq (%rdi)      , %rdi   # 48 85 3f
#   testq (%r8)       , %r8    # 4d 85 00
#   testq (%r15)      , %r15   # 4d 85 3f
#   testq $0x12345678 , %rax   # 48 a9 78 56 34 12
#   testq $0x12345678 , %rdi   # 48 f7 c7 78 56 34 12
#   testq $0x12345678 , %r8    # 49 f7 c0 78 56 34 12
#   testq $0x12345678 , %r15   # 49 f7 c7 78 56 34 12
#   testq (1)         , %rax   # 48 85 04 25 01 00 00 00
#   testq (1)         , %rdi   # 48 85 3c 25 01 00 00 00
#   testq (1)         , %r8    # 4c 85 04 25 01 00 00 00
#   testq (1)         , %r15   # 4c 85 3c 25 01 00 00 00
#   testq $0x12345678 , (%rax) # 48 f7 00 78 56 34 12
#   testq $0x12345678 , (%rdi) # 48 f7 07 78 56 34 12
#   testq $0x12345678 , (%r8)  # 49 f7 00 78 56 34 12
#   testq $0x12345678 , (%r15) # 49 f7 07 78 56 34 12
#   testq %rax        , (%rax) # 48 85 00
#   testq %rdi        , (%rdi) # 48 85 3f
#   testq %r8         , (%r8)  # 4d 85 00
#   testq %r15        , (%r15) # 4d 85 3f
#   testq (%rax)      , (%rax) # ILLEGAL
#   testq (1)         , (%rdi) # ILLEGAL
#   testq $0x12345678 , (1)    # 48 85 04 25 01 00 00 00
#   testq %rax        , (1)    # 48 85 04 25 01 00 00 00
#   testq %rdi        , (1)    # 48 85 3c 25 01 00 00 00
#   testq %r8         , (1)    # 4c 85 04 25 01 00 00 00
#   testq %r15        , (1)    # 4c 85 3c 25 01 00 00 00

#   pushq $0x12345678          # 68 78 56 34 12
#   pushq %rax                 # 50
#   pushq %rcx                 # 51
#   pushq %rdx                 # 52
#   pushq %rdi                 # 57
#   pushq %r8                  # 41 50
#   pushq %r15                 # 41 57
#   pushq (%rax)               # ff 30
#   pushq (%rcx)               # ff 31
#   pushq (%rdx)               # ff 32
#   pushq (%rdi)               # ff 37
#   pushq (%r8)                # 41 ff 30
#   pushq (%r15)               # 41 ff 37
#   pushq (1)                  # ff 34 25 01 00 00 00

#   popq  $0x12345678          # ILLEGAL
#   popq  %rax                 # 58
#   popq  %rcx                 # 59
#   popq  %rdx                 # 5a
#   popq  %rdi                 # 5f
#   popq  %r8                  # 41 58
#   popq  %r15                 # 41 5f
#   popq  (%rax)               # 8f 00
#   popq  (%rcx)               # 8f 01
#   popq  (%rdx)               # 8f 02
#   popq  (%rdi)               # 8f 07
#   popq  (%r8)                # 41 8f 00
#   popq  (%r15)               # 41 8f 07
#   popq  (1)                  # 8f 04 25 01 00 00 00

#   divq  $0x12345678          # ILLEGAL
#   divq  %rax                 # 48 f7 f0
#   divq  %rcx                 # 48 f7 f1
#   divq  %rdx                 # 48 f7 f2
#   divq  %rdi                 # 48 f7 f7
#   divq  %r8                  # 49 f7 f0
#   divq  %r15                 # 49 f7 f7
#   divq  (%rax)               # 48 f7 30
#   divq  (%rcx)               # 48 f7 31
#   divq  (%rdx)               # 48 f7 32
#   divq  (%rsp)               # 48 f7 34 24
#   divq  (%rdi)               # 48 f7 37
#   divq  (%r8)                # 49 f7 30
#   divq  (%r15)               # 49 f7 37
#   divq  (1)                  # 48 f7 34 25 01 00 00 00

#   incq  $0x12345678          # ILLEGAL
#   incq  %rax                 # 48 ff c0
#   incq  %rcx                 # 48 ff c1
#   incq  %rdx                 # 48 ff c2
#   incq  %rdi                 # 48 ff c7
#   incq  %r8                  # 49 ff c0
#   incq  %r15                 # 49 ff c7
#   incq  (%rax)               # 48 ff 00
#   incq  (%rcx)               # 48 ff 01
#   incq  (%rdx)               # 48 ff 02
#   incq  (%rsp)               # 48 ff 04 24
#   incq  (%rdi)               # 48 ff 07
#   incq  (%r8)                # 49 ff 00
#   incq  (%r15)               # 49 ff 07
#   incq  (1)                  # 48 ff 04 25 01 00 00 00

#   decq  $0x12345678          # ILLEGAL
#   decq  %rax                 # 48 ff c8
#   decq  %rcx                 # 48 ff c9
#   decq  %rdx                 # 48 ff ca
#   decq  %rdi                 # 48 ff cf
#   decq  %r8                  # 49 ff c8
#   decq  %r15                 # 49 ff cf
#   decq  (%rax)               # 48 ff 08
#   decq  (%rcx)               # 48 ff 09
#   decq  (%rdx)               # 48 ff 0a
#   decq  (%rsp)               # 48 ff 0c 24
#   decq  (%rdi)               # 48 ff 0f
#   decq  (%r8)                # 49 ff 08
#   decq  (%r15)               # 49 ff 0f
#   decq  (1)                  # 48 ff 0c 25 01 00 00 00

#   callq $0xffffffffffffffff  # ILLEGAL
#   call  %rax                 # ff d0
#   call  %rcx                 # ff d1
#   call  %rdx                 # ff d2
#   call  %rdi                 # ff d7
#   call  %r8                  # 41 ff d0
#   call  %r15                 # 41 ff d7
#   call  (%rax)               # ff 10
#   call  (%rcx)               # ff 11
#   call  (%rdx)               # ff 12
#   call  (%rsp)               # ff 14 24
#   call  (%rdi)               # ff 17
#   call  (%r8)                # 41 ff 10
#   call  (%r15)               # 41 ff 17
#   call  (1)                  # e8 2d ff bf ff
#   call  (0x0000)             # e8 4b ff bf ff
#   call  (0x1234)             # e8 7f 11 c0 ff
#   call  (0xffff)             # e8 45 ff c0 ff
#   callq (0x0000000000000000) # e8 4b ff bf ff 00 00 00
#   callq (0xffffffffffffffff) # e8 4a ff bf ff 00 00 00
#   callq (1)                  # e8 4c ff bf ff
#   callq (2)                  # e8 48 ff bf ff
#   callq (3)                  # e8 44 ff bf ff
#   callq (-1)                 # e8 4a ff bf ff
#   callq (-2)                 # e8 44 ff bf ff
#   callq (-3)                 # e8 3e ff bf ff
#   callq (6)                  # e8 51 ff bf ff
#   callq (-6)                 # e8 40 ff bf ff
#   call  (0xfffff632)
