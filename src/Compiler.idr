module Compiler

import Control.Monad.Either
import Control.Monad.State
import Data.SortedMap
import Data.Vect
import Debug.Trace
import Prim
import Syntax

%default total

%access public export

infixr 3 ..
(..) : (c -> d) -> (a -> b -> c) -> a -> b -> d
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
  symtab : SortedMap String Nat
  mainfn : String

data CompilerError
  = NoFunction String
  | NoMainFunction

X86 : Type -> Type
X86 = EitherT CompilerError (State Mem)

commit : Instr -> Block
commit i = modify (\bs => record { instrs $= ((::) i)
                                 , offset $= (+ (length (codegen i))) } bs )

push : Val -> Block; push = commit . Push
pop  : R_M -> Block; pop  = commit . Pop
div  : R_M -> Block; div  = commit . Div
inc  : R_M -> Block; inc  = commit . Inc
dec  : R_M -> Block; dec  = commit . Dec

mov  : Val -> R_M -> Block; mov  = commit .. Mov
add  : Val -> R64 -> Block; add  = commit .. Add
sub  : Val -> R64 -> Block; sub  = commit .. Sub
test : Val -> R_M -> Block; test = commit .. Test

jmp  : Ptr -> Block; jmp = commit . Jmp
jz   : Ptr -> Block; jz  = commit . Jz
jnz  : Ptr -> Block; jnz = commit . Jnz

ret  : Block; ret = commit Ret
ker  : Block; ker = commit Ker

||| Calls the function labeled by a String, the function must have been declared.
||| TODO: could we statically check that the function has been declared?
call : String -> Block
call = commit . Call . FRef

||| Stores bytes in the data segment
store : List Bits8 -> X86 Nat
store bs =
  do s <- get
     let off = datOff s
     modify $ \m => record { dataBs $= (++) bs, datOff = off} m
     pure off

||| returns a pointer to local offset of a block
local : State BlockState Ptr
local = gets (Loc . fromInteger . cast . offset)

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
epilogue = ret; -- pure () --do pop rdi; ret

data TExpr
  = TSym (Symbol,  TType)
  | TApp (TExpr,   TType) (TExpr, TType)
  | TLam (Symbol,  TType) (TExpr, TType)
  | TLet (Symbol,  TType) (TExpr, TType) (TExpr, TType)
  | TLit (Literal, TType)
  | TIf  (TExpr,   TType) (TExpr, TType) (TExpr, TType)

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
empty = MkMem [] [] [] 0 0 empty empty empty "main"

interface Adjustable a where
  adjv : Int -> Int -> a -> a

Adjustable Val where
  adjv coff doff (P (Code x)) = A (x + coff)
  adjv coff doff (P (PData x)) = A (x + doff)
  adjv coff doff (P (IData x)) = I (x + doff)
  adjv _ _ x = x

Adjustable R_M where
  adjv coff doff (P' (Code x)) = A' (x + coff)
  adjv coff doff (P' (PData x)) = A' (x + doff)
  adjv coff doff (P' (IData x)) = assert_unreachable
  adjv _ _ x = x

Adjustable Ptr where
  adjv coff _ (Abs x) = Abs (x + coff)
  adjv _ _ x = x

adj : Int -> Int -> Instr -> Instr
adj coff doff (Mov  x y) = Mov  (adjv coff doff x) (adjv coff doff y)
adj coff doff (Add  x y) = Add  (adjv coff doff x) y
adj coff doff (Sub  x y) = Sub  (adjv coff doff x) y
adj coff doff (Test x y) = Test (adjv coff doff x) (adjv coff doff y)
adj coff doff (Push x  ) = Push (adjv coff doff x)
adj coff doff (Pop  x  ) = Pop  (adjv coff doff x)
adj coff doff (Lit  x  ) = Lit  x
adj coff doff (Div  x  ) = Div  (adjv coff doff x)
adj coff doff (Inc  x  ) = Inc  (adjv coff doff x)
adj coff doff (Dec  x  ) = Dec  (adjv coff doff x)
adj coff doff (Jmp  x  ) = Jmp  (adjv coff doff x)
adj coff doff (Jz   x  ) = Jz   (adjv coff doff x)
adj coff doff (Jnz  x  ) = Jnz  (adjv coff doff x)
adj coff doff (Call x  ) = Call x
adj coff doff Ret       = Ret
adj coff doff Ker       = Ker
-- adj off (Shl  x y) = Shl  (adjv off x) (adjv off y)

write : List Bits8 -> X86 ()
write bytes =
  modify (\m => record { memOff $= (+ (length bytes))
                       , codeBs $= (\bs => bs ++ bytes) } m)

emitf : Instr -> X86 ()
emitf (Call (FRef name)) = do
  Just address <- gets (lookup name . symtab)
               | Nothing => throwErr (NoFunction name)
  offset <- gets memOff
  write (codegen (Call (MRel ((cast address) - (5 + (cast offset))))))
emitf x = write (codegen x)

emit : Nat -> Nat -> String -> List Instr -> X86 ()
emit coff doff name instrs =
  do offset <- gets memOff
     traverse (emitf . adj (cast coff) (cast doff)) instrs
     modify (\m => record { symtab $= insert name offset } m)

Symtab : Type
Symtab = SortedMap String Nat

mkSymtab : String -> Nat ->  SortedMap String (List Instr, Nat) -> (Symtab, Nat)
mkSymtab main len blocks =
  let acc = (fromList [(main, 0)], len) in
    foldl offset acc (toList blocks)
  where
    offset : (Symtab, Nat) -> (String, (List Instr, Nat)) -> (Symtab, Nat)
    offset (tab, off) (name, (_, len)) = (insert name off tab, off + len)

preCompile : X86 () -> X86 ()
preCompile m = do
  m
  main <- gets mainfn
  blks <- gets blocks
  case lookup main blks of
    Nothing           => throwErr NoMainFunction
    Just (block, len) =>
      let blks' = delete main blks
          (stab, doff) = mkSymtab main len blks'
          coff  = 232 -- current program header size
          doff' = 0x600000 + coff + doff in
        do modify (\m => record { symtab = stab } m)
           emit coff doff' main block
           foldl (\m, (k, v) => do m; emit coff doff' k v)
                 (pure ())
                 (toList (map fst blks'))

runCompile : X86 () -> Either CompilerError (List Bits8, List Bits8)
runCompile m =
  let m' = preCompile m
  in case runState (runEitherT m') empty of
    (Left error, s) => Left error
    (Right    a, s) => Right (codeBs s, dataBs s)

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
