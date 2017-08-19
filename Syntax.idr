module Syntax

import Control.Monad.State
import Data.SortedSet
import Data.SortedMap

data Literal
  = LInt Integer
  | LBool Bool

data Symbol = MkSymbol String

Eq Symbol where
  (MkSymbol x) == (MkSymbol y) = x == y

Ord Symbol where
  compare (MkSymbol x) (MkSymbol y) = compare x y

data Expr
  = Sym Symbol
  | App Expr Expr
  | Lam Symbol Expr
  | Let Symbol Expr Expr
  | Lit Literal
  | If Expr Expr Expr

data TType
  = TVar Symbol
  | TCon String
  | TArr TType TType

free : TType -> SortedSet Symbol
free (TVar x)   = insert x empty
free (TCon x)   = empty
free (TArr a b) = union (free a) (free b)

occurs : Symbol -> TType -> Bool
occurs x term = contains x $ free term

data Sub = MkSub (SortedMap Symbol TType)

interface Subst a where
  sub : Sub -> a -> a

Subst TType where
  sub (MkSub s) (TVar t)   = fromMaybe (TVar t) $ lookup t s
  sub _         (TCon t)   = TCon t
  sub s         (TArr a b) = TArr (sub s a) (sub s b)

empty : Sub
empty = MkSub empty

singleton : Symbol -> TType -> Sub
singleton s t = MkSub $ fromList [(s, t)]

comp : Sub -> Sub -> Sub
comp s (MkSub t) = MkSub $ map (sub s) t

data TErr
  = ErrInfiniteType
  | ErrArity
  | ErrUnification TType TType
  | ErrUnboundVariable

record EitherT (l : Type) (m : Type -> Type) (r : Type) where
  constructor MkEitherT
  runEitherT : m (Either l r)

implementation MonadTrans (EitherT e) where
  lift x = MkEitherT $ do a <- x; pure $ Right a

implementation Functor f => Functor (EitherT a f) where
  map f = MkEitherT . map (map f) . runEitherT

implementation Monad f => Applicative (EitherT a f) where
  pure = MkEitherT . pure . pure
  (MkEitherT f) <*> (MkEitherT a) =
    MkEitherT $ do b <- a
                   g <- f
                   case g of
                     Left err => pure $ Left err
                     Right h  => pure $ map h b

implementation Monad m => Monad (EitherT a m) where
  m >>= k = MkEitherT $ do a <- runEitherT m
                           case a of
                             Left  l => pure (Left l)
                             Right r => runEitherT (k r)

implementation MonadState s m => MonadState s (EitherT e m) where
  get = lift get
  put = lift . put

Infer : Type -> Type
Infer a = EitherT TErr (State Nat) a

runInfer : Infer (Sub, TType) -> Either TErr (Sub, TType)
runInfer m =
  case evalState (runEitherT m) 0 of
    Left err  => Left err
    Right res => Right res

throwErr : Monad m => TErr -> EitherT TErr m a
throwErr x = MkEitherT $ pure $ Left x

bind : Symbol -> TType -> Infer Sub
bind s t@(TVar a) = pure $ if s == a then empty else singleton s t
bind s t          = if occurs s t
                    then throwErr ErrInfiniteType
                    else pure $ singleton s t

vars : Stream Symbol
vars = [ MkSymbol $ singleton c ++ show n | n <- nums, c <- cycle ['a'..'z'] ]
  where inc26 : (Nat, Nat) -> (Nat, Nat)
        inc26 (i, x) = if i < 26 then (i + 1, x) else (0, x + 1)
        nums : Stream Nat
        nums = map snd $ iterate inc26 (0, 0)

fresh : Infer TType
fresh = do i <- get; modify (plus 1); pure $ TVar $ index i vars

unify : TType -> TType -> EitherT TErr (State Nat) Sub
unify (TArr a b) (TArr a' b') =
  do s1 <- unify a a'
     s2 <- unify (sub s1 b) (sub s1 b')
     pure $ comp s2 s1

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) with ((==) a b) | True = pure empty
unify t1 t2 = throwErr $ ErrUnification t1 t2

data Scheme = Forall (SortedSet Symbol) TType

instantiate : Scheme -> Infer TType
instantiate (Forall as t) =
  let as' = Data.SortedSet.toList as
  in do as'' <- traverse (const fresh) as'
        let s = MkSub $ fromList $ zip as' as''
        pure $ sub s t
