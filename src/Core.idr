module Core

import Compiler
import Control.Monad.Either
import Control.Monad.State
import Prim

%default total

%access public export

p__add_int64 : X86 ()
p__add_int64 =
  primfn "p__add_int64" $ do
    mov rdi rax
    add r8  rax

p__add_lit_int64 : Int -> Int -> Block
p__add_lit_int64 x y = do
  mov (I x) rdi
  mov (I y) r8
  call "p__add_int64"

sys_brk : Val
sys_brk = (I 12)

||| Allocates bytes passed in rdi, returns pointer to start of allocated memory
||| in rax; clobbers rax.
p__alloc : X86 ()
p__alloc =
  primfn "p__alloc" $ do
    push rcx         -- sav rcx, or it will get clobbered
    push rdi         -- sav arg rdi
    mov sys_brk rax  -- sys_brk
    mov (I 0) rdi    -- xor rdi rdi clear rdi
    syscall          -- syscall
    pop rdi          -- restore arg rdi
    push rax         -- save ptr
    add rax rdi      -- add ptr to arg rdi = new alloc end
    mov sys_brk rax  -- sys_brk
    syscall          -- systall
    pop rax          -- restore ptr back in rax
    pop rcx          -- restore rcx

||| put a number in rdi, returns in rsi how many bytes the number is long
p__byte_len : X86 ()
p__byte_len =
  primfn "p_byte_len" $ do
    mov  rdx      rax
    mov  (I 0x00) rcx
    mov  (I 0xff) rbx
    while (test (I 0) rax) $ do
      div  rbx
      add  (I 1) rcx
    mov  rcx rsi

||| put a number in rdi, returns in rsi a pointer to the ascii string
||| representation of that number
p__to_str_int64 : X86 ()
p__to_str_int64 =
  primfn "p__to_str_int64" $ do
    mov  rdi      rax -- put arg rdi in rax
    mov  (I   10) rbx -- put lit 10 in rbx

    whileNot (test rax rax) $ do
      mov  (I  0) rdx -- clear rdx
      div  rbx        -- div rax by rbx
      add  (I 48) rdx -- add 48 for ascii conversion
      push rdx        -- push remainder onto stack
      inc  rcx        -- inc counter

    mov  rcx   rdi  -- put counter in rdi
    call "p__alloc" -- allocate number of bytes in rdi
    mov  rcx   rdi  -- put counter in rdi
    mov  rax   r8   -- put ptr in r8
    mov  rax   rbx  -- put ptr to allocated string in rbx
    mov  (I 0) rax  -- clear rax

    whileNot (test rcx rcx) $ do
      pop  rdx         -- get char off stack -> rdx
      mov  rdx _rbx    -- move char to address in rbx
      dec  rcx         -- decrement counter
      inc  rbx         -- inc address to point to next byte of string

p__prn : X86 ()
p__prn =
  primfn "p__prn" $ do
    mov rdi   rdx
    mov r8    rcx
    mov (I 1) rbx
    mov (I 4) rax
    ker

-- ||| the pointers to a & b must have already been assigned/allocated, a or b
-- ||| could be infinite/recursive, and so sz will not return a meaningful value.
-- p__pair : X86 ()
-- p__pair =
--   do label "p__pair"
--      fst' <- map cast $ alloc 4
--      snd' <- map cast $ alloc 4
--      mov (R  Rdi) (A fst')
--      mov (R   R8) (A snd')
--      mov (P fst') (R  Rsi)
--      ret

p__exit : X86 ()
p__exit =
  primfn "p__exit" $ do
    mov (I 0) rbx
    mov (I 1) rax
    ker

p__prn_lit_str : String -> X86 Block
p__prn_lit_str s = do
  p <- store $ map fromChar $ unpack s
  let len = (I $ cast $ length s)
  pure $ do mov  len   rdi
            mov  (P (IData (cast p))) r8
            call "p__prn"

p__prn_int64 : X86 ()
p__prn_int64 =
  primfn "p__prn_int64" $ do
    call "p__to_str_int64"
    call "p__prn"

p__prn_lit_int64 : Int -> X86 Block
p__prn_lit_int64 i = pure $ do
  mov  (I i) rdi         -- put i in rdi
  call "p__prn_int64"

helloWorld : X86 ()
helloWorld = do
  f <- p__prn_lit_str "Hello, world!\n"
  primfn "helloWorld" $ do
    f
    call "p__exit"

_main : X86 ()
_main = do
  primfn "main" $ do
    p__add_lit_int64 2 2   -- result in rax
    mov rax rdi            -- pass to call
    call "p__to_str_int64" -- to_str
    call "p__prn"
    call "p__exit"
  p__alloc
  p__add_int64
  p__to_str_int64
  p__exit
  p__prn
  start "main"

prog : X86 ()
prog = do p__alloc; p__exit; p__prn; helloWorld; start "helloWorld"
