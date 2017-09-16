module Compiler

import Control.Monad.State
import Data.SortedMap
import Data.Vect
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
  nextid : Nat
  fwdlup : SortedMap Nat Nat

X86 : Type -> Type
X86 = State Mem

codeSize : X86 Int
codeSize = do
  is <- gets instrs
  pure $ foldl1 (+) $ map instrSize is

emit : Instr -> X86 ()

emit (Jz (MkPtr ptr)) = do
  offset <- gets (cast . memOff)
  let diff = offset - ptr
  if diff <= 127 then
     emit $ SJz (I (0xff - diff))
     else pure ()

emit i =
  let bs = reverse $ toBs8 i
  in do offset <- gets memOff
        let offset' = offset + length bs
        modify $ \m => record { codeBs $= (++) bs
                              , memOff = offset'
                              , instrs $= (::) i } m

store : List Bits8 -> X86 Nat
store bs =
  do s <- get
     let off = datOff s
     modify $ \m => record { dataBs $= (++) bs, datOff = off} m
     pure off

||| returns a pointer to the current code offset in memory
ptr : X86 Ptr -- returns a ptr?
ptr = map (MkPtr . cast) $ gets memOff

fwd : X86 Ptr
fwd = do
  i <- gets nextid
  modify (\m => record { nextid $= (+ 1) } m)
  pure (Fwd i)

rlz : Ptr -> X86 ()
rlz (Fwd p) = do
  offset <- gets memOff
  modify (\m => record { fwdlup $= insert p offset } m)
rlz _ = assert_unreachable


||| labels the current code offset in the symtab, and returns a pointer to it
label : String -> X86 Ptr -- returns a ptr?
label s = do
  offset <- gets memOff
  modify (\m => record { symtab $= insert s offset } m)
  pure (MkPtr (cast offset))

||| prologue is responsible for preparing a function call. It needs to handle
||| its arguments and push the stack frame on entry.
prologue : X86 ()
prologue = emit $ Push (R Rdi)

||| epilogue is responsible for clearing up after a function call. It needs to
||| pop the stack frame on entry and handle the return
epilogue : X86 ()
epilogue = emit $ Pop (R Rdi)

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
empty = MkMem [] [] [] 0 0 empty empty 0 empty

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
      sz = cast $ 0x600000 + 232 + (length $ concatMap toBs8 is)
      -- ^^ could just compile once, and record the position of where offsets
      -- need to be adjusted, then adjust the bytes
  in (concatMap toBs8 $ map (adj sz) $ reverse is, dataBs s)

p__add_int64 : X86 ()
p__add_int64 =
  do prologue
     emit $ Mov (R Rdi) (R Rax)
     emit $ Add (R R8)  (R Rax)
     epilogue

p__add_lit_int64 : Int -> Int -> X86 ()
p__add_lit_int64 x y =
  do emit $ Mov (I x) (R Rdi)
     emit $ Mov (I y) (R R8)
     p__add_int64

-- always keep the current heap offset in Rbp
-- heap starts in data section
||| Puts current heap pointer in Rsi, 'allocates' no of bytes in int Rdi
allocHeap : X86 ()
allocHeap =
  do emit $ Mov (R Rbp) (R Rsi)
     emit $ Add (R Rdi) (R Rbp)

||| put a number in rdi, returns in rsi how many bytes the number is long
p__byte_len : X86 ()
p__byte_len =
  do emit $ Mov (R  Rdi) (R Rax)
     emit $ Mov (I 0x00) (R Rcx)
     emit $ Mov (I 0xff) (R Rbx)
     loop <- ptr
     emit $ Div  (R Rbx)
     emit $ Add  (I   1) (R Rcx)
     emit $ Test (I   0) (R Rax)
     emit $ Jz loop
     emit $ Mov  (R Rcx) (R Rsi)

||| put a number in rdi, returns in rsi a pointer to the ascii string
||| representation of that number
p__to_str_int64 : X86 ()
p__to_str_int64 =
  do label "p__to_str_int64"
     emit $ Mov  (R  Rdi) (R Rax)
     emit $ Mov  (I   10) (R Rbx)
     loop1 <- ptr
     emit $ Div  (R  Rbx)
     emit $ Add  (I 0x30) (R Rdx) -- add 0x30 for ascii conversion
     emit $ Push (R  Rdx)
     emit $ Inc  (R Rcx)
     emit $ Test (I    0) (R Rax)
     emit $ Jz loop1
     -- emit $ Mov  (I   0) (R Rax) -- must already be 0
     emit $ Mov  (R  Rcx) (R Rdi) -- put counter in rdi
     allocHeap                    -- to allocate number of bytes
     loop2 <- ptr
     next <- fwd                  -- forward decl pointer
     emit $ Pop  (R  Rdx)         -- get char off stack -> rdx
     emit $ Add  (R  Rdx) (R Rax) -- add to rax
     emit $ Test (I    0) (R Rcx) -- check if counter is zero?
     emit $ Jz next               -- if true, jump out of loop to rlz next
     emit $ Shl  (I    8) (R Rax) -- else bitshift rax 1 byte left
     emit $ Dec  (R Rcx)          -- decrement counter
     rlz next

-- get int input, @ rdi
-- divide by 10, quot is in rax, rem is in rdx
-- put rem onto 8 bit stack
-- inc count (rcx?) - length of string
-- loop until 0
-- pop stack, print string

p__prn : X86 (Maybe Nat)
p__prn =
  do prologue
     emit $ Mov (R Rdi) (R Rdx)
     emit $ Mov (R R8)  (R Rcx)
     emit $ Mov (I 1) (R Rbx)
     emit $ Mov (I 4) (R Rax)
     epilogue
     emit Ker
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

exit : X86 (Maybe Nat)
exit = do emit $ Mov (I 0) (R Rbx)
          emit $ Mov (I 1) (R Rax)
          emit Ker
          pure $ Just 0

p__prn_lit_str : String -> X86 (Maybe Nat)
p__prn_lit_str s =
  do p <- map cast $ store $ map fromChar $ unpack s
     emit $ Mov (I $ cast $ length s) (R Rdi)
     emit $ Mov (P p) (R R8)
     p__prn

helloWorld : X86 (Maybe Nat)
helloWorld =
  do p__prn_lit_str "Hello, world!\n"
     exit
