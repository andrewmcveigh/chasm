import Control.Monad.State

%hide Ptr
%hide Prelude.Stream.(::)

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

data Instr = Mov Val Val
           | Jmp Val

record Mem where
  constructor MkMem
  instrs : List Instr
  dataBs : List Bits8
  memOff : Int

X86 : Type -> Type
X86 = State Mem

emit : List Bits8 -> X86 ()
emit bs = modify $ \m => record { dataBs $= (++) bs } m

label : X86 Val -- returns a ptr?
label = map A $ gets memOff

data Lit = LInt Integer
         | LBool Bool
         | LString String
         | LChar Char

data Expr = Symbol String
          | Keyword String
          | Literal Lit
          | Seq (List Expr)

||| prologue is responsible for preparing a function call. It needs to handle
||| its arguments and push the stack frame on entry.
prologue : Nat -> List Instr
-- where do we even get the args from? or that info? I suppose that we should
-- have that info from the compiler?
-- if we first restrict ourselves to single arg functions, do we need any more
-- info?
-- Arguments must come from somewhere. - where?
-- Either they are literal values, or computed values. If literal, they exist at
-- a known location in memory, if computed they will exist at some location in
-- memory

-- would it be a good idea now to define a simple grammar, and review what the
-- goal is?

||| epilogue is responsible for clearing up after a function call. It needs to
||| pop the stack frame on entry and handle the return
epilogue : Nat -> List Instr

-- a lexical binding is a pointer to a pointer?
lookupLexicalBindings : Expr -> X86 (Maybe Val)
lookupLexicalBindings (Symbol name) = pure $ Just $ A 0
lookupLexicalBindings _ = pure Nothing

lookupFunction : Expr -> X86 (Maybe Val)

-- just the type sig, forward declaration
compile : Expr -> X86 (Maybe (List Instr))

callFn : Expr -> List Expr -> X86 (Maybe (List Instr))
callFn op args =
  do op'   <- compile op
     args' <- map concat $ traverse compile args
     case (op', args') of
       (Just op', Just args') => let arity = length args' in
                                     pure $ Just $ concat [ (prologue arity)
                                                          , op' ++ args'
                                                          , (epilogue arity)]
       _ => pure Nothing

ifThenElseExpr : List Expr -> X86 (Maybe (List Instr))
ifThenElseExpr [test, consequent, alternative] =
  do label'       <- map Jmp label
     test'        <- compile test
     consequent'  <- compile consequent
     alternative' <- compile alternative
     let conc = (liftA2 (++))
     pure $ conc (Just [label']) (conc (conc test' consequent') alternative')

makeLambda : List Expr -> X86 (Maybe (List Instr))
makeLambda [name, (Seq [arg1]), expr] =
  do name' <- map Jmp label
     expr' <- compile expr
     pure $ (liftA2 (++)) (Just [name']) expr'

-- compile : Expr -> X86 (Maybe (List Instr))
compile name@(Symbol _) =
  do reg <- lookupLexicalBindings name
     case reg of
       Just reg => pure $ Just [Mov reg rax]
       Nothing  => do label <- lookupFunction name
                      case label of
                        Just label => pure $ Just [Jmp label]
                        Nothing    => pure Nothing

compile (Seq (op :: args)) =
  case op of
    (Symbol "if") => ifThenElseExpr args
    (Symbol "fn") => makeLambda args
    _             => callFn op args

compile _ = pure Nothing


-- extend adds a binding to the env - is it even needed
-- extend : Symbol -> X86 ()

-- callFn op args

-- mov : Val -> Val -> Instr
-- mov (R src) (R dst) =

ret : X86 ()
ret = do
  emit [0xc3]
