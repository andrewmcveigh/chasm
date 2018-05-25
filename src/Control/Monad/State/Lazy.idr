module Control.Monad.State.Lazy

record State (s : Type) (a : Type) where
  constructor ST
  runState : Lazy s -> Lazy (a, s)

implementation Functor (State s) where
  map f (ST g) = ST (\s => let (a, s') = g s in (f a, s'))

implementation Applicative (State s) where
  pure x = ST (\s => (x, s))
  (ST f) <*> (ST a) = ST (\st => let (g, r) = f st
                                     (b, t) = a r
                                 in (g b, t))

implementation Monad (State s) where
  (ST sf) >>= f = ST $ \s => let (a, s') = sf s in runState (f a) s'

get : State s s
get = ST (\s => (s, s))

modify : (s -> s) -> State s ()
modify f = ST (\s => ((), f s))

put : s -> State s ()
put = modify . const

evalState : State s a -> s -> a
evalState m s = fst (runState m s)

interface Monad m => MonadFix (m : Type -> Type) where
  mfix : (Lazy a -> m a) -> m a

implementation MonadFix (State s) where
  mfix f = ST g where
    g s = res where
      res = let (ST sf) = (f (fst res)) in sf s

-- fibs : List Int
-- fibs = mfix (scanl (+) 0 . (\x => 1::x))

fix : (Lazy a -> a) -> a
fix f = x where x = f x

badhead : List a -> Lazy a
badhead [] = assert_unreachable
badhead (x :: _) = x

badtail : List a -> List a
badtail [] = assert_unreachable
badtail (_ :: xs) = xs

implementation MonadFix List where
  mfix f = case fix (f . badhead . Force) of
             []    => []
             (x::_) => x :: mfix (badtail . f)

range : List Int
range = fix (map (+1) . (\x => -1::x))

lst : List Int
lst = mfix $ \x => [x + 1]

implementation MonadFix Maybe where
    mfix f = x where
      x = f (fromJust x) where
        fromJust (Just y) = y
        fromJust Nothing  = assert_unreachable

-- range : State (List Int) Int
-- range = mfix $ \x => do
--   xs <- get
--   put (x::xs)
--   pure (x + 1)

-- useRange : State (List Int) ()
-- useRange = do
--   range
--   xs <- get
--   put (take 5 xs)

-- ff : List (Int, Int)
-- ff = mfix $ \b' => do
--   a <- [1, 2, snd b']
--   b <- [3, 4]
--   pure (a, b)
