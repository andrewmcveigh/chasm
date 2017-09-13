module Prim

import Control.Monad.State
import Data.Bits
import Data.SortedMap
import Syntax

%access public export

%hide Ptr
%hide Prelude.Stream.(::)

data TExpr
  = TSym (Symbol,  TType)
  | TApp (TExpr,   TType) (TExpr, TType)
  | TLam (Symbol,  TType) (TExpr, TType)
  | TLet (Symbol,  TType) (TExpr, TType) (TExpr, TType)
  | TLit (Literal, TType)
  | TIf  (TExpr,   TType) (TExpr, TType) (TExpr, TType)

(>>) : Monad m => m a -> m b -> m b
(>>) ma mb = ma >>= \_ => mb

szt : TType -> Nat
szt (TCon "Int") = 64
szt (TArr a b)   = szt a + szt b

sz : TExpr -> Nat
sz (TSym (_, t)) = szt t

NULL : Integer
NULL = 0x600000

data R64 = Rax | Rcx | Rdx | Rbx | Rsp | Rbp | Rsi | Rdi
         | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15

data Val = I Int | R R64 | A Int | P Int

r64index : R64 -> Integer
r64index r = case r of
  Rax => 0;  Rcx => 1;  Rdx => 2;  Rbx => 3;
  Rsp => 4;  Rbp => 5;  Rsi => 6;  Rdi => 7
  R8  => 8;  R9  => 9;  R10 => 10; R11 => 11;
  R12 => 12; R13 => 13; R14 => 14; R15 => 15

rax : Val; rax = R Rax; r8  : Val; r8  = R R8
rcx : Val; rcx = R Rcx; r9  : Val; r9  = R R9
rdx : Val; rdx = R Rdx; r10 : Val; r10 = R R10
rbx : Val; rbx = R Rbx; r11 : Val; r11 = R R11
rsp : Val; rsp = R Rsp; r12 : Val; r12 = R R12
rbp : Val; rbp = R Rbp; r13 : Val; r13 = R R13
rsi : Val; rsi = R Rsi; r14 : Val; r14 = R R14
rdi : Val; rdi = R Rdi; r15 : Val; r15 = R R15

regToReg : Bits8 -> R64 -> R64 -> List Bits8
regToReg op rx ry =
  let ix = r64index rx
      iy = r64index ry
  in if ix < 8 && iy < 8
     then [0x48, op, fromInteger $ 0xc0 + 8 * ix + iy]
     else if ix < 8 && iy >= 8
     then [0x49, op, fromInteger $ 0xc0 + 8 * ix + (iy - 8)]
     else if ix >= 8 && iy < 8
     then [0x4c, op, fromInteger $ 0xc0 + 8 * (ix - 8) + iy]
     else [0x4d, op, fromInteger $ 0xc0 + 8 * (ix - 8) + (iy - 8)]

bs : Integer -> Integer -> List Bits8
bs 8 = reverse . b64ToBytes . fromInteger
bs 4 = reverse . b32ToBytes . fromInteger
bs 2 = reverse . b16ToBytes . fromInteger
bs 1 = reverse . b8ToBytes  . fromInteger
bs _ = assert_unreachable

opcode : R64 -> Bits8
opcode = fromInteger . r64index

toReg : Bits8 -> List Bits8 -> R64 -> List Bits8
toReg op val reg =
  let i = r64index reg
  in if i < 8
     then [0x48, op, fromInteger (0xc0 + i)] ++ val
     else [0x49, op, fromInteger (0xc0 + i - 8)] ++ val

derefReg : Integer -> R64 -> List Bits8
derefReg op reg =
  let i   = r64index reg
  in if i < 8
     then [fromInteger (op + i)]
     else [0x41, fromInteger (op + (i - 8))]

regToMem : R64 -> Integer -> List Bits8
regToMem r ptr =
  let r' = (0xc0 + (r64index r))
  in if r' < 8
     then 0x48 :: 0x4b :: fromInteger (0x04 + (8 * r')) :: 0x25 :: bs 8 ptr
     else 0x4c :: 0x4b :: fromInteger (0x04 + (8 * (r' - 8))) :: 0x25 :: bs 8 ptr

data Instr = Mov  Val Val
           | Jmp  Val
           | Push Val
           | Pop  Val
           | Ret -- [0xc3]
           | Ker -- [0xcd 0x80]
           | Lit  Integer
           | Add  Val Val
           | Sub  Val Val

toBs8 : Instr -> List Bits8
toBs8 (Mov (R src) (R dst)) = regToReg 0x89 src dst
toBs8 (Mov (I val) (R dst)) = toReg 0xc7 (bs 8 (cast val)) dst
toBs8 (Mov (R src) (A dst)) = regToMem src (cast dst)
toBs8 (Mov (A src) (R dst)) = toReg 0xc7 (bs 8 (cast src)) dst
toBs8 (Add (R src) (R dst)) = regToReg 0x01 src dst
toBs8 (Sub (R src) (R dst)) = regToReg 0x29 src dst
-- all cases done?

toBs8 (Push (R r)) = derefReg 0x50 r
toBs8 (Pop  (R r)) = derefReg 0x58 r

toBs8 (Jmp  (R r)) =
  let i   = r64index r
  in if i < 8
     then [0xff, fromInteger (0xe0 + i)]
     else [0x41, 0xff, fromInteger (0xe0 + (i - 8))]

toBs8 (Jmp  (A a)) = 0xff :: 0x25 :: bs 8 (cast a)

toBs8 (Lit val) = bs 8 val

toBs8 Ret = [0xc3]
toBs8 Ker = [0xcd, 0x80]

-- toBs8 (Add (I val) (R Rax)) = 0x48 :: 0x05 :: (bs 8 (cast val)) -- up to 32bit
-- toBs8 (Add (I val) (R dst)) = toReg 0x81 (bs 8 (cast val)) dst  -- up to 32bit
toBs8 _ = []

||| Max byte size of an instruction is 17 bytes. More accuracy can be added per
||| instruction type and operand type.
instrSize : Instr -> Int
instrSize Ret = 1
instrSize Ker = 2
instrSize _ = 17
