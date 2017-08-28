import Control.Monad.State
import Data.SortedMap
import Syntax

%hide Ptr
%hide Prelude.Stream.(::)

(>>) : Monad m => m a -> m b -> m b
(>>) ma mb = ma >>= \_ => mb

NULL : Integer
NULL = 0x600000

data R64 = Rax | Rcx | Rdx | Rbx | Rsp | Rbp | Rsi | Rdi
         | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15

data Val = I Int | R R64 | A Int

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

regToReg : Bits8 -> R64 -> R64 -> (Bits8, Bits8, Bits8)
regToReg op rx ry =
  let ix = r64index rx
      iy = r64index ry
  in if ix < 8 && iy < 8
     then (0x48, op, fromInteger $ 0xc0 + 8 * ix + iy)
     else if ix < 8 && iy >= 8
     then (0x49, op, fromInteger $ 0xc0 + 8 * ix + (iy - 8))
     else if ix >= 8 && iy < 8
     then (0x4c, op, fromInteger $ 0xc0 + 8 * (ix - 8) + iy)
     else (0x4d, op, fromInteger $ 0xc0 + 8 * (ix - 8) + (iy - 8))

data Instr = Mov  Val Val
           | Jmp  Val
           | Push Val
           | Pop  Val
           | Ret -- [0xc3]
           | Nop  Val

record Mem where
  constructor MkMem
  instrs : List Instr
  codeBs : List Bits8
  dataBs : List Bits8
  memOff : Int
  datOff : Nat
  symtab : SortedMap String Nat
  envtab : SortedMap Symbol Nat
  astack : List Symbol

X86 : Type -> Type
X86 = State Mem

-- This should go into program memory, but what is that?
emit : List Bits8 -> X86 ()
emit bs = modify $ \m => record { codeBs $= (++) bs } m

emitInstr : Instr -> X86 ()
emitInstr i = modify $ \m => record { instrs $= (::) i } m

store : List Bits8 -> X86 Nat
store bs = do s <- get
              let off = length bs + datOff s
              _ <- modify $ \m => record { dataBs $= (++) bs, datOff = off} m
              pure off

label : X86 Val -- returns a ptr?
label = map A $ gets memOff

||| prologue is responsible for preparing a function call. It needs to handle
||| its arguments and push the stack frame on entry.
prologue : Val -> X86 ()
prologue v = (emitInstr $ Push (R Rdi)) >> emitInstr (Mov v $ R Rdi)
-- ^^ maybe we don't do the mov here?

||| epilogue is responsible for clearing up after a function call. It needs to
||| pop the stack frame on entry and handle the return
epilogue : X86 ()
epilogue = emitInstr (Pop (R Rdi)) >> emitInstr Ret

-- a lexical binding is a pointer to a pointer?
lookupLexicalBindings : Symbol -> X86 (Maybe Val)
lookupLexicalBindings name = pure $ Just $ A 0

lookupFunction : Symbol -> X86 (Maybe Val)
lookupFunction s = gets ((map (A . cast)) . (lookup s) . envtab)

lookupSymbol : Symbol -> X86 Nat
lookupSymbol (MkSymbol s) =
  do st <- get
     case lookup s $ symtab st of
       Nothing => store $ map fromChar $ unpack s
       Just p  => pure p
  where fromChar : Char -> Bits8
        fromChar c = fromInteger $ cast $ ord c

alloc : Nat -> X86 Nat
alloc size = do off <- gets memOff
                modify (\s => record { memOff $= (+ (cast size)) } s)
                pure $ cast off

compile : Expr -> X86 (Maybe Nat)

-- Compiles a symbol. What does that even mean?
-- Well, 2 symbols with the same name should be the same thing. So firstly this
-- is a lookup to the symbol table, if the symbol hasn't already been created,
-- it creates the symbol, and returns the symbol's offset in the data section of
-- memory
-- This is useful for symbols that are symbols at runtime. Symbols in source,
-- should be looked up according to evaluation rules.
compile (Sym name) = -- the (Maybe Nat) is the address of the thing.
  do st <- gets envtab
     case lookup name st of
       Just a  => emitInstr (Mov (A (cast a)) (R Rdi)) >> pure (Just a)
       Nothing => pure Nothing
-- ^^ this also really depends on what type of thing the value at Rdi is, no?
-- if it's a string, we want to put a pointer to the string in Rdi
-- if it's a function, we want to put a pointer to the function in Rdi
-- if it's a ..., ...
-- where do we get the pointer from?
-- if it's an argument, do we just want to know that we need to look in Rdi
-- it doesn't really matter if it's an argument, or a fn, we want to put it
-- somewhere

-- Compiles a function application. What is that?
-- There is executable code somewhere in memory. We should have an address for
-- that block of memory. We should also have one argument to the executable.
-- code this is either a literal value, or an address to some computed value.
-- This address or value needs moving into a register where the executable code
-- can access it. The value that was already in the register needs saving, and
-- putting back at the start and end of the executable blcok respectively.
compile (App op arg) =
  do op'  <- compile op -- should add instructions, and return address/label
     arg' <- compile arg -- ditto, return address/label
     case (op', arg') of
       (Just op', Just arg') => do _ <- prologue $ A $ cast arg'
                                   _ <- emitInstr $ Jmp $ A $ cast op'
                                   _ <- epilogue
                                   pure Nothing

-- Compiles a lambda.
-- Lambdas are partially evaluated at compile time. They need symbol lookup
-- checks, macroexpansion, type-checking, and potentially inlining of
-- expressions that we can statically evaluate.
-- a lambda is anonymous
-- get the offset
-- set up binding
---- we know there is going to be an argument passed in
---- there may be a reference to the binding
---- there will be a pointer to a value in RDI
-- emit a bunch of code
-- return offset
compile (Lam x e) =
  do ptrx <- alloc 64
     etab <- gets envtab
     modify (\s => record { envtab $= insert x ptrx } s)
     compile e
     modify (\s => record { envtab = etab } s)
     pure $ Just $ ptrx + 64

-- compile (Let b e1 e2) = ?undefined

compile (Lit (LInt x)) = (emitInstr $ Nop $ I x) >> pure Nothing

-- Compiles an if then else expression.
-- First executes test, then depending on the result, executes then or else
compile (If test consequent alternative) =
  do label'       <- map Jmp label
     test'        <- compile test
     consequent'  <- compile consequent
     alternative' <- compile alternative
     pure Nothing

compile _ = pure Nothing

-- extend adds a binding to the runtime env
-- prim__extend : Val -> Expr -> X86 ()
-- prim__extend sym val =

-- mov : Val -> Val -> Instr
-- mov (R src) (R dst) =
