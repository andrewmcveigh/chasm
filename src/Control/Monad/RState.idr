module Control.Monad.RState

-- import public Control.Monad.Identity
-- import public Control.Monad.Trans

record RState (s : Type) (a : Type) where
  constructor RS
  runRState : Lazy s -> Lazy (a, Lazy s)

implementation Functor (RState s) where
  map f (RS g) = RS (\s => let (a, s') = g s in (f a, s'))

implementation Applicative (RState s) where
  pure x = RS (\s => (x, s))
  (RS f) <*> (RS a) = RS (\st => let (g, r) = f st
                                     (b, t) = a r
                                 in (g b, t))

-- inner : RState s a -> (s -> (a, s)) -> s -> (a, s)
-- inner (RS sf) f s = final where
--   final = runRState (f (fst inter)) s where
--     inter = sf (snd final)

implementation Monad (RState s) where
  (RS sf) >>= f = RS g where
    g : Lazy s -> Lazy (b, Lazy s)
    g state = x where
      x = runRState (f (Basics.fst (sf (Basics.snd x)))) state


    -- (a, s'') = sf s'
--     (RST f) >>= k = RST (\s => do (a, s'') <- f s'
--                                   let RST kv = k a
--                                   kv s)
rget : RState s s
rget = RS (\s => (s, s))

rmodify : (s -> s) -> RState s ()
rmodify f = RS (\s => ((), f s))

rput : s -> RState s ()
rput = rmodify . const

evalRState : RState s a -> s -> a
evalRState m s = fst (runRState m s)

fix : (Lazy a -> a) -> a
fix f = x where x = f x

range : List Int
range = fix (map (+1) . (\x => -1::x))

fibs : List Int
fibs = fix (scanl (+) 0 . (\x => 1::x))

-- interface Monad m => MonadFix (m : Type -> Type) where
--   mfix : (a -> m a) -> m a

-- implementation MonadFix (\a => a -> b) where
--   mfix f = \r => let a = f a r in a

-- implementation (Monad a, Monad b) => MonadFix (Pair a, b) where
--     mfix f = ((mfix (fstP . f)), (mfix (sndP . f))) where
--       fstP : (a, b) -> a
--       fstP = fst
--       sndP : (a, b) -> b
--       sndP = snd

-- implementation MonadFix (RState s) where
--   mfix f = RS $ \s => mfix $ \as => runRState (f (fst as)) (snd as)


-- \r => let a = f a r in a
-- rget = RState $ \s -> (s,s)
-- rmodify f = RState $ \s -> ((),f s)
-- rput = rmodify . const

-- execRState f s = snd (runRState f s)
-- module Control.Monad.State

-- import public Control.Monad.Identity
-- import public Control.Monad.Trans

-- %access public export

-- ||| A computation which runs in a context and produces an output
-- interface Monad m => MonadRState stateType (m : Type -> Type) | m where
--     ||| Get the context
--     get : m stateType
--     ||| Write a new context/output
--     put : stateType -> m ()

-- ||| The transformer on which the State monad is based
-- record RStateT (stateType : Type) (m : Type -> Type) (a : Type) where
--   constructor RST
--   runRStateT : stateType -> m (a, stateType)

-- implementation Functor f => Functor (RStateT stateType f) where
--     map f (RST g) = RST (\st => map (mapFst f) (g st)) where
--        mapFst : (a -> x) -> (a, s) -> (x, s)
--        mapFst fn (a, b) = (fn a, b)

-- implementation Monad f => Applicative (RStateT stateType f) where
--     pure x = RST (\st => pure (x, st))

--     (RST f) <*> (RST a) = RST (\st => do (g, r) <- f st
--                                          (b, t) <- a r
--                                          pure (g b, t))

-- implementation Monad m => Monad (RStateT stateType m) where
--     (RST f) >>= k = RST (\s => do (a, s'') <- f s'
--                                   let RST kv = k a
--                                   kv s)

-- implementation Monad m => MonadRState stateType (RStateT stateType m) where
--     get   = RST (\x => pure (x, x))
--     put x = RST (\y => pure ((), x))

-- implementation MonadTrans (RStateT stateType) where
--     lift x = RST (\st => do r <- x
--                             pure (r, st))

-- ||| Apply a function to modify the context of this computation
-- modify : MonadRState stateType m => (stateType -> stateType) -> m ()
-- modify f = do s <- get
--               put (f s)

-- ||| Evaluate a function in the context held by this computation
-- gets : MonadRState stateType m => (stateType -> a) -> m a
-- gets f = do s <- get
--             pure (f s)

-- ||| The RState monad. See the MonadRState interface
-- RState : (stateType : Type) -> (ty : Type) -> Type
-- RState = \s, a => RStateT s Identity a

-- ||| Unwrap a State monad computation.
-- runRState : RStateT stateType Identity a -> stateType -> (a, stateType)
-- runRState act = runIdentity . runRStateT act

-- ||| Unwrap a State monad computation, but discard the final state.
-- evalRState : RState stateType a -> stateType -> a
-- evalRState m = fst . runRState m

-- ||| Unwrap a State monad computation, but discard the resulting value.
-- execRState : RState stateType a -> stateType -> stateType
-- execRState m = snd . runRState m
