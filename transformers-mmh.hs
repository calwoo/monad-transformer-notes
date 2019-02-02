-- Notes on monad transformers following Monday Morning Haskell: 
--      https://mmhaskell.com/monads-6

import Control.Applicative

-- we use monad transformers to combine monads. Monad transformers are fundamentally wrapper types

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    return x = MaybeT $ fmap return (return x)
    x >>= f = MaybeT $ do
        k <- runMaybeT x
        case k of
            Just y -> runMaybeT $ f y
            Nothing -> return Nothing

-- functor and applicative instances
instance (Monad m) => Functor (MaybeT m) where
    fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance (Monad m) => Applicative (MaybeT m) where
    pure = return
    f <*> x = MaybeT $ liftA2 (<*>)
        (runMaybeT f)
        (runMaybeT x)

-- so MaybeT is a monad!



