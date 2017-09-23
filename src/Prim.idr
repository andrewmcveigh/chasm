module Prim

import Control.Monad.State
import Data.Bits
import Data.SortedMap
import Debug.Trace
import Syntax

%default total

%access public export

%hide Ptr
%hide Prelude.Stream.(::)

(>>) : Monad m => m a -> m b -> m b
(>>) ma mb = ma >>= \_ => mb

NULL : Integer
NULL = 0x600000

data R64 = Rax | Rcx | Rdx | Rbx | Rsp | Rbp | Rsi | Rdi
         | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15

idx : R64 -> Integer
idx r = case r of
  Rax => 0;  Rcx => 1;  Rdx => 2;  Rbx => 3;
  Rsp => 4;  Rbp => 5;  Rsi => 6;  Rdi => 7
  R8  => 8;  R9  => 9;  R10 => 10; R11 => 11;
  R12 => 12; R13 => 13; R14 => 14; R15 => 15

data Pt  = Code Int | Data Int

data Val = I Int | R  R64 | D  R64 | A  Int | P  Pt
data R_M =         R' R64 | D' R64 | A' Int | P' Pt

data Ptr = Loc Bits8 | Rel Int | Abs Int
data Fp  = FRef String | MRel Int

R64.rax : R64; R64.rax = Rax; R64.r8  : R64; R64.r8  = R8
R64.rcx : R64; R64.rcx = Rcx; R64.r9  : R64; R64.r9  = R9
R64.rdx : R64; R64.rdx = Rdx; R64.r10 : R64; R64.r10 = R10
R64.rbx : R64; R64.rbx = Rbx; R64.r11 : R64; R64.r11 = R11
R64.rsp : R64; R64.rsp = Rsp; R64.r12 : R64; R64.r12 = R12
R64.rbp : R64; R64.rbp = Rbp; R64.r13 : R64; R64.r13 = R13
R64.rsi : R64; R64.rsi = Rsi; R64.r14 : R64; R64.r14 = R14
R64.rdi : R64; R64.rdi = Rdi; R64.r15 : R64; R64.r15 = R15

Val.rax : Val; Val.rax = R Rax; Val.r8  : Val; Val.r8  = R R8
Val.rcx : Val; Val.rcx = R Rcx; Val.r9  : Val; Val.r9  = R R9
Val.rdx : Val; Val.rdx = R Rdx; Val.r10 : Val; Val.r10 = R R10
Val.rbx : Val; Val.rbx = R Rbx; Val.r11 : Val; Val.r11 = R R11
Val.rsp : Val; Val.rsp = R Rsp; Val.r12 : Val; Val.r12 = R R12
Val.rbp : Val; Val.rbp = R Rbp; Val.r13 : Val; Val.r13 = R R13
Val.rsi : Val; Val.rsi = R Rsi; Val.r14 : Val; Val.r14 = R R14
Val.rdi : Val; Val.rdi = R Rdi; Val.r15 : Val; Val.r15 = R R15

R_M.rax : R_M; R_M.rax = R' Rax; R_M.r8  : R_M; R_M.r8  = R' R8
R_M.rcx : R_M; R_M.rcx = R' Rcx; R_M.r9  : R_M; R_M.r9  = R' R9
R_M.rdx : R_M; R_M.rdx = R' Rdx; R_M.r10 : R_M; R_M.r10 = R' R10
R_M.rbx : R_M; R_M.rbx = R' Rbx; R_M.r11 : R_M; R_M.r11 = R' R11
R_M.rsp : R_M; R_M.rsp = R' Rsp; R_M.r12 : R_M; R_M.r12 = R' R12
R_M.rbp : R_M; R_M.rbp = R' Rbp; R_M.r13 : R_M; R_M.r13 = R' R13
R_M.rsi : R_M; R_M.rsi = R' Rsi; R_M.r14 : R_M; R_M.r14 = R' R14
R_M.rdi : R_M; R_M.rdi = R' Rdi; R_M.r15 : R_M; R_M.r15 = R' R15

R_M._rax : R_M; R_M._rax = D' Rax; R_M._r8  : R_M; R_M._r8  = D' R8
R_M._rcx : R_M; R_M._rcx = D' Rcx; R_M._r9  : R_M; R_M._r9  = D' R9
R_M._rdx : R_M; R_M._rdx = D' Rdx; R_M._r10 : R_M; R_M._r10 = D' R10
R_M._rbx : R_M; R_M._rbx = D' Rbx; R_M._r11 : R_M; R_M._r11 = D' R11
R_M._rsp : R_M; R_M._rsp = D' Rsp; R_M._r12 : R_M; R_M._r12 = D' R12
R_M._rbp : R_M; R_M._rbp = D' Rbp; R_M._r13 : R_M; R_M._r13 = D' R13
R_M._rsi : R_M; R_M._rsi = D' Rsi; R_M._r14 : R_M; R_M._r14 = D' R14
R_M._rdi : R_M; R_M._rdi = D' Rdi; R_M._r15 : R_M; R_M._r15 = D' R15

bs : Integer -> Int -> List Bits8
bs 8 = reverse . b64ToBytes . fromInteger . cast
bs 4 = reverse . b32ToBytes . fromInteger . cast
bs 2 = reverse . b16ToBytes . fromInteger . cast
bs 1 = reverse . b8ToBytes  . fromInteger . cast
bs _ = assert_unreachable

opcode : R64 -> Bits8
opcode = fromInteger . idx

grid8x8 : Bits8 -> Bits8 -> Integer -> Integer -> Integer -> List Bits8
grid8x8 pfx op start x y = [pfx, op, fromInteger $ start + 8 * x + y]

reginst : Bits8 -> Bits8 -> Integer -> Integer -> Integer -> List Bits8
reginst pfx op start src dst =
  let (pfx' , src') = if src < 8 then (pfx , src) else (pfx  + 4, src - 8)
      (pfx'', dst') = if dst < 8 then (pfx', dst) else (pfx' + 1, dst - 8)
  in grid8x8 pfx'' op start src' dst'

regins1 : Bits8 -> Bits8 -> Integer -> Integer -> List Bits8
regins1 pfx op start reg =
  let (pfx' , reg') = if reg < 8 then (pfx , reg) else (pfx  + 1, reg - 8)
  in grid8x8 pfx' op start 0 reg'

data Instr = Mov  Val   R_M
           | Add  Val   R64
           | Sub  Val   R64
           | Test Val   R_M
           -- | Shl  Bits8 R_M
           | Push Val
           | Pop  R_M
           | Lit  Int
           | Div  R_M
           | Inc  R_M       -- Decrement register
           | Dec  R_M       -- Decrement register
           | Jmp  Ptr       -- Jump to ptr
           | Jz   Ptr       -- Jump if (last result was) zero
           | Jnz  Ptr       -- Jump if (last result was) zero
           | Call Fp        -- "call" a function in the env
           | Ret            -- return & pop
           | Ker            -- call kernel [0xcd 0x80]

literal : Bits8 -> Integer -> Int -> R64 -> List Bits8
literal op start val dst = reginst 0x48 op start 0 (idx dst) ++ bs 4 val

address : Bits8 -> Integer -> Int -> R64 -> List Bits8
address op start add dst = reginst 0x48 op start (idx dst) 0 ++ 0x25 :: bs 4 add

registr : Bits8 -> Integer -> R64 -> R64 -> List Bits8
registr op start src dst = reginst 0x48 op start (idx src) (idx dst)

regist1 : Bits8 -> Integer -> R64 -> List Bits8
regist1 op start reg = regins1 0x48 op start (idx reg)

single1 : Bits8 -> (l : List Integer) -> {auto ok : NonEmpty l} -> R64 -> List Bits8
single1 pfx op reg =
  let i     = idx reg
      start = last op
      butl  = init op
  in if i < 8
     then map fromInteger (butl ++ [start + i])
     else pfx :: map fromInteger (butl ++ [start + i - 8])

codegen : Instr -> List Bits8

codegen (Mov  (I val) (R' dst)) = literal 0xc7 0xc0 val dst
codegen (Mov  (R src) (R' dst)) = registr 0x89 0xc0 src dst
codegen (Mov  (D src) (R' dst)) = registr 0x8b 0x00 src dst
codegen (Mov  (A src) (R' dst)) = address 0x8b 0x04 src dst
codegen (Mov  (I val) (D' dst)) = literal 0xc7 0x00 val dst
codegen (Mov  (R src) (D' dst)) = registr 0x89 0x00 src dst
codegen (Mov  (I val) (A' dst)) = [0x48, 0xc7, 0x04, 0x25] ++ bs 4 dst ++ bs 4 val
codegen (Mov  (R src) (A' dst)) = reginst 0x48 0x89 0x04 (idx src) 0 ++ 0x25 :: bs 4 dst
-- this is the asm generated for ^^ 49 c7 c0 29 01 60 00 # mov 0x00600129 r8
-- which is very similar to literal rule
codegen (Mov  (I val) (P' dst)) = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
codegen (Mov  (R src) (P' dst)) = [0, 0, 0, 0, 0, 0, 0, 0]
codegen (Mov  (P _  ) (R' _  )) = [0, 0, 0, 0, 0, 0, 0, 0]
codegen (Mov  _       _       ) = assert_unreachable

codegen (Add  (I val)     dst ) = literal 0x81 0xc0 val dst
codegen (Add  (R src)     dst ) = registr 0x01 0xc0 src dst
codegen (Add  (D src)     dst ) = registr 0x03 0x00 src dst
codegen (Add  (A src)     dst ) = address 0x03 0x04 src dst
codegen (Add  (P _  )     _   ) = [0, 0, 0, 0, 0, 0, 0, 0]

codegen (Sub  (I val)     dst ) = literal 0x81 0xe8 val dst
codegen (Sub  (R src)     dst ) = registr 0x29 0xc0 src dst
codegen (Sub  (D src)     dst ) = registr 0x2b 0x00 src dst
codegen (Sub  (A src)     dst ) = address 0x2b 0x04 src dst
codegen (Sub  (P _  )     _   ) = [0, 0, 0, 0, 0, 0, 0, 0]

codegen (Test (I val) (R' dst)) = literal 0xf7 0xc0 val dst
codegen (Test (R src) (R' dst)) = registr 0x85 0xc0 src dst
codegen (Test (D src) (R' dst)) = registr 0x85 0x00 src dst
codegen (Test (A src) (R' dst)) = address 0x85 0x04 src dst
codegen (Test (I val) (D' dst)) = literal 0xf7 0x00 val dst
codegen (Test (R src) (D' dst)) = registr 0x85 0x00 src dst
codegen (Test (I val) (A' dst)) = [0x48, 0xf7, 0x04, 0x25] ++ bs 4 dst ++ bs 4 val
codegen (Test (R src) (A' dst)) = reginst 0x48 0x85 0x04 (idx src) 0 ++ 0x25 :: bs 4 dst
codegen (Test (I _  ) (P' _  )) = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
codegen (Test (R _  ) (P' _  )) = [0, 0, 0, 0, 0, 0, 0, 0]
codegen (Test (P _  ) (R' _  )) = [0, 0, 0, 0, 0, 0, 0, 0]
codegen (Test _       _       ) = assert_unreachable

codegen (Push (I  val)) = 0x68 :: bs 4 val
codegen (Push (R  reg)) = single1 0x41 [0x50] reg
codegen (Push (D  reg)) = single1 0x41 [0xff, 0x50] reg
codegen (Push (A  add)) = [0xff, 0x34, 0x25] ++ bs 4 add
codegen (Push (P  _  )) = [0, 0, 0, 0, 0, 0, 0]

codegen (Pop  (R' reg)) = single1 0x41 [0x58] reg
codegen (Pop  (D' reg)) = single1 0x41 [0x8f, 0x00] reg
codegen (Pop  (A' add)) = [0x8f, 0x04, 0x25] ++ bs 4 add
codegen (Pop  (P' _  )) = [0, 0, 0, 0, 0, 0, 0]

codegen (Div  (R' reg)) = regist1 0xf7 0xf0 reg
codegen (Div  (D' Rsp)) = [0x48, 0xf7, 0x34, 0x24]
codegen (Div  (D' reg)) = regist1 0xf7 0x30 reg
codegen (Div  (A' add)) = [0x48, 0xf7, 0x34, 0x25] ++ bs 4 add
codegen (Div  (P' _  )) = [0, 0, 0, 0, 0, 0, 0]

codegen (Inc  (R' reg)) = regist1 0xff 0xc0 reg
codegen (Inc  (D' Rsp)) = [0x48, 0xff, 0x04, 0x24]
codegen (Inc  (D' reg)) = regist1 0xff 0x00 reg
codegen (Inc  (A' add)) = [0x48, 0xff, 0x04, 0x25] ++ bs 4 add
codegen (Inc  (P' _  )) = [0, 0, 0, 0, 0, 0, 0]

codegen (Dec  (R' reg)) = regist1 0xff 0xc8 reg
codegen (Dec  (D' Rsp)) = [0x48, 0xff, 0x0c, 0x24]
codegen (Dec  (D' reg)) = regist1 0xff 0x08 reg
codegen (Dec  (A' add)) = [0x48, 0xff, 0x0c, 0x25] ++ bs 4 add
codegen (Dec  (P' _  )) = [0, 0, 0, 0, 0, 0, 0]

codegen (Jmp  (Loc b )) = [0xeb, b]
codegen (Jmp  (Rel i )) = 0xe9 :: bs 4 i
codegen (Jmp  (Abs i )) = 0xff :: 0x25 :: bs 4 i

codegen (Jz   (Loc b )) = [0x74, b]
codegen (Jz   (Rel i )) = 0x0f :: 0x84 :: bs 4 i
codegen (Jz   (Abs i )) = assert_unreachable

codegen (Jnz  (Loc b )) = [0x75, b]
codegen (Jnz  (Rel i )) = 0x0f :: 0x84 :: bs 4 i
codegen (Jnz  (Abs i )) = assert_unreachable

codegen (Call (FRef s)) = [0, 0, 0, 0, 0]
codegen (Call (MRel i)) = 0xe8 :: bs 4 i -- TODO: maybe this should only be 2b

codegen (Lit val) = bs 4 val
codegen Ret       = [0xc3]
codegen Ker       = [0xcd, 0x80]
