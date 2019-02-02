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
-- We can use this monad in situations where we combine IO and Maybe

-- examples)
readUserName :: MaybeT IO String
readUserName = MaybeT $ do
    str <- getLine
    if length str > 5
        then return $ Just str
        else return Nothing

readPassword :: MaybeT IO String
readPassword = MaybeT $ do
    pwd <- getLine
    if length pwd < 6
        then return Nothing
        else return $ Just pwd

main :: IO ()
main = do
    maybeCreds <- runMaybeT $ do
        usr <- readUserName
        pwd <- readPassword
        return (usr, pwd)
    case maybeCreds of
        Nothing -> putStrLn "Couldn't login!"
        Just (u, p) -> putStrLn "Verifying..."

-- what does this code look like without the monad transformer?

{- main :: IO ()
main = do
    maybeUsername <- readUserName
    case maybeUsername of
        Nothing -> putStrLn "Invalid name!"
        Just uName -> do
            maybePassword <- readPassword
            case maybePassword of
                Nothing -> putStrLn "Invalid pass!"
                Just uPass -> putStrLn "Verifying..."
 -}

-- pretty bad.

-- now as MaybeT is again a monad, we can wrap it in another monad! Hence we can get stacks of monad
-- transformers...

type Env = (Maybe String, Maybe String, Maybe String)

readUserName' :: MaybeT (ReaderT Env IO) String
readUserName'