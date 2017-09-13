module Compiler

import Control.Monad.State
import Data.SortedMap
import Prim
import Syntax

%access public export

record Mem where
  constructor MkMem
  instrs : List Instr
  codeBs : List Bits8
  dataBs : List Bits8
  memOff : Nat
  datOff : Nat
  symtab : SortedMap String Nat
  envtab : SortedMap Symbol Nat
  astack : List Symbol

X86 : Type -> Type
X86 = State Mem

codeSize : X86 Int
codeSize = do
  is <- gets instrs
  pure $ foldl1 (+) $ map instrSize is

emit : Instr -> X86 Nat
emit i =
  let bs = reverse $ toBs8 i
  in do offset <- gets memOff
        let offset' = offset + length bs
        modify $ \m => record { codeBs $= (++) bs
                              , memOff = offset'
                              , instrs $= (::) i } m
        pure offset'

store : List Bits8 -> X86 Nat
store bs =
  do s <- get
     let off = length bs + datOff s
     modify $ \m => record { dataBs $= (++) bs, datOff = off} m
     pure off

label : X86 Val -- returns a ptr?
label = map (A . cast) $ gets memOff

||| prologue is responsible for preparing a function call. It needs to handle
||| its arguments and push the stack frame on entry.
prologue : X86 ()
prologue = (emit $ Push (R Rdi)) >> pure ()

||| epilogue is responsible for clearing up after a function call. It needs to
||| pop the stack frame on entry and handle the return
epilogue : X86 ()
epilogue = (emit $ Pop (R Rdi)) >> emit Ret >> pure ()

-- a lexical binding is a pointer to a pointer?
lookupLexicalBindings : Symbol -> X86 (Maybe Val)
lookupLexicalBindings name = pure $ Just $ A 0

lookupFunction : Symbol -> X86 (Maybe Val)
lookupFunction s = gets ((map (A . cast)) . (lookup s) . envtab)

fromChar : Char -> Bits8
fromChar c = fromInteger $ cast $ ord c

lookupSymbol : Symbol -> X86 Nat
lookupSymbol (MkSymbol s) =
  do st <- get
     case lookup s $ symtab st of
       Nothing => store $ map fromChar $ unpack s
       Just p  => pure p

alloc : Nat -> X86 Nat
alloc size = do off <- gets memOff
                modify (\s => record { memOff $= (+ size)
                                     , codeBs $= (++) (take size (repeat 0)) } s)
                pure off

compile : TExpr -> X86 (Maybe Nat)

-- Compiles a symbol. What does that even mean?
-- Well, 2 symbols with the same name should be the same thing. So firstly this
-- is a lookup to the symbol table, if the symbol hasn't already been created,
-- it creates the symbol, and returns the symbol's offset in the data section of
-- memory
-- This is useful for symbols that are symbols at runtime. Symbols in source,
-- should be looked up according to evaluation rules.
compile (TSym (name, t)) = -- the (Maybe Nat) is the address of the thing.
  do st <- gets envtab
     case lookup name st of
       Just a  => emit (Mov (A (cast a)) (R Rdi)) >> pure (Just a)
       Nothing => pure Nothing

-- Compiles a function application. What is that?
-- There is executable code somewhere in memory. We should have an address for
-- that block of memory. We should also have one argument to the executable.
-- code this is either a literal value, or an address to some computed value.
-- This address or value needs moving into a register where the executable code
-- can access it. The value that was already in the register needs saving, and
-- putting back at the start and end of the executable blcok respectively.
compile (TApp (op, _) (arg, _)) =
  do op'  <- compile op  -- should add instructions, and return address/label
     arg' <- compile arg -- ditto, return address/label
     case (op', arg') of -- this should be different depending on the type
       (Just op', Just arg') =>
         do prologue
            emit $ Mov (A $ (cast op') - 64) (A $ cast arg')
            emit $ Jmp $ A $ cast op'
            epilogue
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
compile (TLam (x, _) (e, _)) =
  do ptrx <- alloc 4
     etab <- gets envtab
     modify (\s => record { envtab $= insert x ptrx } s)
     compile e
     modify (\s => record { envtab = etab } s)
     pure $ Just $ ptrx + 64

-- compile (Let b e1 e2) =
--   do off  <- gets memOff
--      etab <- gets envtab
--      ptre <- compile e1
--      modify (\s => record { envtab $= insert b ptre } s)
--      complie e2
--      pure $ Just off

compile (TLit ((LInt x), _)) = (emit $ Lit $ cast x) >> pure Nothing

-- Compiles an if then else expression.
-- First executes test, then depending on the result, executes then or else
compile (TIf (test, _) (consequent, _) (alternative, _)) =
  do label'       <- map Jmp label
     test'        <- compile test
     consequent'  <- compile consequent
     alternative' <- compile alternative
     pure Nothing

compile _ = pure Nothing

defaultOffset : Int
defaultOffset = 0x600000

prgoff : Int
prgoff = (232 + 44)

empty : Mem
empty = MkMem [] [] [] 0 0 empty empty []

adjVal : Int -> Val -> Val
adjVal offset (P x) = A $ x + offset
adjVal _ x = x

adj : Int -> Instr -> Instr
adj off (Mov x y) = Mov  (adjVal off x) (adjVal off y)
adj off (Jmp x)   = Jmp  (adjVal off x)
adj off (Push x)  = Push (adjVal off x)
adj off (Pop x)   = Pop  (adjVal off x)
adj off Ret       = Ret
adj off Ker       = Ker
adj off (Lit x)   = Lit  x
adj off (Add x y) = Add  (adjVal off x) (adjVal off y)
adj off (Sub x y) = Sub  (adjVal off x) (adjVal off y)

runCompile : X86 (Maybe Nat) -> (List Bits8, List Bits8)
runCompile m =
  let (a, s) = runState m empty
      is = instrs s
      sz = foldl1 (+) $ map instrSize is
  in (concatMap toBs8 $ map (adj sz) $ reverse is, dataBs s)

p__add : X86 Nat
p__add =
  do prologue
     x <- alloc 4
     y <- alloc 4
     emit $ Mov (A $ cast x) (R Rax)
     emit $ Mov (A $ cast y) (R Rdi)
     emit $ Add (R Rdi) (R Rax)
     epilogue
     pure (y + 64)

p__prn : X86 (Maybe Nat)
p__prn =
  do prologue
     emit $ Mov (R Rdi) (R Rdx)
     emit $ Mov (R R8)  (R Rcx)
     emit $ Mov (I 1) (R Rbx)
     emit $ Mov (I 4) (R Rax)
     emit Ker
     epilogue
     pure Nothing

||| the pointers to a & b must have already been assigned/allocated, a or b
||| could be infinite/recursive, and so sz will not return a meaningful value.
p__pair : X86 (Maybe Nat)
p__pair =
  do prologue
     fst' <- alloc 4
     snd' <- alloc 4
     emit $ Mov (R Rdi) (A $ cast fst')
     emit $ Mov (R R8)  (A $ cast snd')
     epilogue
     pure $ Just fst'

helloWorld : X86 (Maybe Nat)
helloWorld =
  let s = "Hello, World!\n"
  in do p <- map cast $ store $ map fromChar $ unpack s
        emit $ Mov (I $ cast $ length s) (R Rdx)
        emit $ Mov (P p) (R Rcx)
        p__prn
