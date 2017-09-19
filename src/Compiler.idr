module Compiler

import Control.Monad.Either
import Control.Monad.State
import Data.SortedMap
import Data.Vect
import Prim
import Syntax

%access public export

infixr 3 ..
(..) : (b -> c) -> (a -> a -> b) -> a -> a -> c
(..) g f = \x, y => g (f x y)

infixr 3 >=>
(>=>) : Monad m => m a -> m a -> m a
(>=>) ma mb = ma >>= (\_ => mb)

record BlockState where
  constructor MkBlockState
  instrs : List Instr
  offset : Nat

Block : Type
Block = State BlockState ()

record Mem where
  constructor MkMem
  instrs : List Instr
  codeBs : List Bits8
  dataBs : List Bits8
  memOff : Nat
  datOff : Nat
  envtab : SortedMap Symbol Nat
  blocks : SortedMap String (List Instr, Nat)
  mainfn : String

data CompilerError
  = NoFunction String
  | NoMainFunction

X86 : Type -> Type
X86 = EitherT CompilerError (State Mem)

commit : Instr -> Block
commit i = modify (\bs => record { instrs $= ((::) i)
                                 , offset $= (+ (length (toBs8 i))) } bs )

push : Val -> Block; push = commit . Push
pop  : Val -> Block; pop  = commit . Pop
div  : Val -> Block; div  = commit . Div
inc  : Val -> Block; inc  = commit . Inc
dec  : Val -> Block; dec  = commit . Dec

mov  : Val -> Val -> Block; mov  = commit .. Mov
add  : Val -> Val -> Block; add  = commit .. Add
sub  : Val -> Val -> Block; sub  = commit .. Sub
test : Val -> Val -> Block; test = commit .. Test

jmp  : Ptr -> Block; jmp = commit . Jmp
jz   : Ptr -> Block; jz  = commit . Jz
jnz  : Ptr -> Block; jnz = commit . Jnz

ret  : Block; ret = commit Ret
ker  : Block; ker = commit Ker

||| Calls the function labeled by a String, the function must have been declared.
||| TODO: could we statically check that the function has been declared?
call : String -> Block
call = commit . Call . F

||| Stores bytes in the data segment
store : List Bits8 -> X86 Nat
store bs =
  do s <- get
     let off = datOff s
     modify $ \m => record { dataBs $= (++) bs, datOff = off} m
     pure off

||| returns a pointer to local offset of a block
local : State BlockState Ptr
local = gets (Local . cast . offset)

fromChar : Char -> Bits8
fromChar c = fromInteger $ cast $ ord c

zeros : Nat -> List Bits8
zeros n = (take n (repeat 0))

alloc : Nat -> X86 Nat
alloc size = do
  off <- gets memOff
  modify (\s => record { memOff $= (+ size), codeBs $= (++) (zeros size) } s)
  pure off

||| prologue is responsible for preparing a function call. It needs to handle
||| its arguments and push the stack frame on entry.
prologue : Block
prologue = pure () --push rdi

||| epilogue is responsible for clearing up after a function call. It needs to
||| pop the stack frame on entry and handle the return
epilogue : Block
epilogue = pure () --do pop rdi; ret

compile : TExpr -> X86 (Maybe Nat)

-- Compiles a symbol. What does that even mean?
-- Well, 2 symbols with the same name should be the same thing. So firstly this
-- is a lookup to the symbol table, if the symbol hasn't already been created,
-- it creates the symbol, and returns the symbol's offset in the data section of
-- memory
-- This is useful for symbols that are symbols at runtime. Symbols in source,
-- should be looked up according to evaluation rules.
-- compile (TSym (name, t)) = -- the (Maybe Nat) is the address of the thing.
--   do st <- gets envtab
--      case lookup name st of
--        Just a  => emit (Mov (A (cast a)) (R Rdi)) >> pure (Just a)
--        Nothing => pure Nothing

-- Compiles a function application. What is that?
-- There is executable code somewhere in memory. We should have an address for
-- that block of memory. We should also have one argument to the executable.
-- code this is either a literal value, or an address to some computed value.
-- This address or value needs moving into a register where the executable code
-- can access it. The value that was already in the register needs saving, and
-- putting back at the start and end of the executable blcok respectively.

-- compile (TApp (op, _) (arg, _)) =
--   do op'  <- compile op  -- should add instructions, and return address/label
--      arg' <- compile arg -- ditto, return address/label
--      case (op', arg') of -- this should be different depending on the type
--        (Just op', Just arg') =>
--          do prologue
--             emit $ Mov (A $ (cast op') - 64) (A $ cast arg')
--             emit $ Jmp $ A $ cast op'
--             epilogue
--             pure Nothing

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

-- compile (TLit ((LInt x), _)) = (emit $ Lit $ cast x) >> pure Nothing

-- -- Compiles an if then else expression.
-- -- First executes test, then depending on the result, executes then or else
-- compile (TIf (test, _) (consequent, _) (alternative, _)) =
--   do label'       <- map (Jmp . cast) ptr
--      test'        <- compile test
--      consequent'  <- compile consequent
--      alternative' <- compile alternative
--      pure Nothing

compile _ = pure Nothing

defaultOffset : Int
defaultOffset = 0x600000

prgoff : Int
prgoff = (232 + 44)

empty : Mem
empty = MkMem [] [] [] 0 0 empty empty "main"

adjVal : Int -> Val -> Val
adjVal offset (P x) = A $ x + offset
adjVal _ x = x

adj : Int -> Instr -> Instr
adj off (Mov  x y) = Mov  (adjVal off x) (adjVal off y)
adj off (Push x  ) = Push (adjVal off x)
adj off (Pop  x  ) = Pop  (adjVal off x)
adj off (Lit  x  ) = Lit  x
adj off (Add  x y) = Add  (adjVal off x) (adjVal off y)
adj off (Sub  x y) = Sub  (adjVal off x) (adjVal off y)
adj off (Div  x  ) = Div  (adjVal off x)
adj off (Test x y) = Test (adjVal off x) (adjVal off y)
adj off (Shl  x y) = Shl  (adjVal off x) (adjVal off y)
adj off (Inc  x  ) = Inc  (adjVal off x)
adj off (Dec  x  ) = Dec  (adjVal off x)
-- adj off (Jmp  x  ) = Jmp  (adjVal off x)
-- adj off (Jz   x  ) = Jz   (adjVal off x)
-- adj off (Jnz  x  ) = Jnz  (adjVal off x)
adj off (SJz  x  ) = SJz  (adjVal off x)
adj off (NJz  x  ) = NJz  (adjVal off x)
adj off (AJz  x  ) = AJz  (adjVal off x)
adj off (Call x  ) = Call (adjVal off x)
adj off Ret       = Ret
adj off Ker       = Ker

runCompile : X86 () -> Either CompilerError (List Bits8, List Bits8)
runCompile m =
  case runState (runEitherT m) empty of
    (Left error, _) => Left error
    (Right    _, s) =>
      let main = mainfn s
          blks = blocks s
      in case lookup main blks of
        Nothing          => Left NoMainFunction
        Just (mainBlock, len) => let blks = delete main blks in
          Right (concatMap toBs8 (reverse mainBlock), dataBs s)

          -- is = instrs s
          -- sz = cast $ 0x600000 + 232 + (length $ concatMap toBs8 is)
      -- ^^ could just compile once, and record the position of where offsets
      -- need to be adjusted, then adjust the bytes

primfn : String -> Block -> X86 ()
primfn name block =
  let (_, s) = runState (do prologue; block; epilogue) (MkBlockState [] 0)
  in modify (\m => record { blocks $= insert name (instrs s, offset s) } m )

loop : Block -> Block
loop block =
  do here <- local
     block
     jmp here

while : Block -> Block -> Block
while test body =
  do here <- local
     body
     test
     jz here

whileNot : Block -> Block -> Block
whileNot test body =
  do here <- local
     body
     test
     jnz here

start : String -> X86 ()
start name = modify (\m => record { mainfn = name } m)
