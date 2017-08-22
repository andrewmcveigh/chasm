module Control.Monad.Either

import Control.Monad.State

%access public export
%default total

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

throwErr : Monad m => err -> EitherT err m a
throwErr = MkEitherT . pure . Left

catchError : Monad m => EitherT e m a -> (e -> EitherT e m a) -> EitherT e m a
catchError m h = MkEitherT $ do
    a <- runEitherT m
    case a of
        Left  l => runEitherT $ h l
        Right r => pure $ Right r
