-- Notes on monad transformers following "A gentle introduction to monad transformers"
--      https://two-wrongs.com/a-gentle-introduction-to-monad-transformers

-- Suppose we want to build a function that pulls the domain out of a email addy.

-- NB: We're working with Text here, not strings

{-# LANGUAGE OverloadedStrings #-}

import Data.Text

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map
import Control.Applicative

data LoginError = InvalidEmail | NoSuchUser | WrongPassword
    deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
    case splitOn "@" email of
        [name, domain] -> Right domain
        _ -> Left InvalidEmail

-- So what do we do to apply further computation to this? We could just use pattern matching.
printResult' :: Either LoginError Text -> IO ()
printResult' domain =
    case domain of
        Right text -> T.putStrLn (append "Domain: " text)
        Left InvalidEmail -> T.putStrLn "ERROR: Invalid domain"

-- A slightly better way relies on the universal property of coproducts, which is encoded in the
-- either function, either :: (a -> c) -> (b -> c) -> (Either a b -> c)
printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . either
        (const "ERROR: Invalid domain")
        (append "Domain: ")

-- Okay, cool. Now lets try to think of the domain as a "user token", ie the value the user uses to
-- prove they have authenticated.

getToken :: IO (Either LoginError Text)
getToken = do
    T.putStrLn "Please input your email: "
    input <- T.getLine
    return $ getDomain input

-- Now lets complete our verification system with an authentication system (ie, passwords)
-- Here are some sample users.

users :: Map Text Text
users = Data.Map.fromList 
    [("example.com", "hahaha"), ("localhost", "homotopy")]

userLogin :: IO (Either LoginError Text)
userLogin = do
    token <- getToken
    case token of
        Right domain ->
            case Data.Map.lookup domain users of
                Just userpw -> do
                    T.putStrLn "Enter password: "
                    pwd <- T.getLine
                    if pwd == userpw then
                        return token
                    else
                        return $ Left WrongPassword
                Nothing ->
                    return $ Left NoSuchUser
        left -> return left

-- this works, but is ugly
-- why is it ugly? We're trying to mix Either and IO together, but they don't really
-- blend well.


-- monads don't compose! but what if we had a single monad that combined both IO and
-- error handing?

data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
} -- this is the datatype we keep seeing here.

-- Lets get it to be a monad.
instance Functor (EitherIO e) where -- this is stupid. composition of functors are functors
    fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
    pure = EitherIO . pure . pure
    -- (<*>) :: EitherIO e (a -> b) -> EitherIO e a -> EitherIO e b
    f <*> x = EitherIO $ liftA2 (<*>)
        (runEitherIO f)
        (runEitherIO x)

{- instance Applicative (EitherIO e) where
    pure = EitherIO . pure . pure
    -- (<*>) :: EitherIO e (a -> b) -> EitherIO e a -> EitherIO e b
    f <*> x = wrapped
        where
            unwrapf = runEitherIO f
            unwrapx = runEitherIO x
            comp = liftA2 (<*>) unwrapf unwrapx
            wrapped = EitherIO $ comp -}
        
instance Monad (EitherIO e) where
    return = pure
    x >>= f = EitherIO $ 
        runEitherIO x >>= either
            (return . Left)
            (runEitherIO . f)

-- Okay! We have a real monad!

-- Can we use it to rewrite the above? Lets refactor a bit.
getToken' :: EitherIO LoginError Text
getToken' = do
    EitherIO $ fmap pure (T.putStrLn "Enter email address: ")
    input <- EitherIO (fmap pure T.getLine)
    EitherIO (return $ getDomain input)

-- **sobs** but this is WORSE!
-- lets first define a way to lift IO/Either into EitherIO
liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO $ fmap pure x

getTokenv2 :: EitherIO LoginError Text
getTokenv2 = do
    liftIO $ T.putStrLn "Enter email address: "
    input <- liftIO T.getLine
    liftEither $ getDomain input
-- hmm, looks a bit better

userLoginv2 :: EitherIO LoginError Text
userLoginv2 = do
    token <- getTokenv2
    userpw <- maybe (liftEither $ Left NoSuchUser)
        return (Data.Map.lookup token users) 
        -- maybe : b -> (a -> b) -> Maybe a -> b
        -- <default-val> -> f -> if nothing, default, else run f
    password <- liftIO (
        T.putStrLn "Enter your password: " >>
        T.getLine)
    if userpw == password then
        return token
    else
        liftEither $ Left WrongPassword

-- the nesting is gone!
printResultv2 :: Either LoginError Text -> IO ()
printResultv2 res =
    T.putStrLn $ case res of
        Right token ->
            append "Logged in with token: " token
        Left InvalidEmail ->
            "Invalid email address entered."
        Left NoSuchUser ->
            "No user with that email exists."
        Left WrongPassword ->
            "Wrong password."

-- However, our code is still pretty ugly as we have all these
-- explicit liftings around. Doesn't look all that natural.

-- we can throw errors using a function
throwE :: e -> EitherIO e a
throwE x = liftEither $ Left x

userLoginv3 :: EitherIO LoginError Text
userLoginv3 = do
    token <- getTokenv2
    userpw <- maybe (throwE NoSuchUser)
        return (Data.Map.lookup token users)
    password <- liftIO (
        T.putStrLn "Enter your password: " >>
        T.getLine)
    if userpw == password then
        return token
    else
        throwE WrongPassword
-- wow, this is much more readable!

-- Really, all we needed from Either was a way to talk about exceptions, and 
-- EitherIO was a way to talk about exceptions and IO together.

-- DIGRESSION: since we can throw exceptions, we can catch them too, right?
catchE :: EitherIO e a -> (e -> EitherIO e a) -> EitherIO e a
catchE throwing handler =
    EitherIO $ do
        result <- runEitherIO throwing
        case result of
            Left failure -> runEitherIO $ handler failure
            success -> return success

wrongPasswordHandler :: LoginError -> EitherIO LoginError Text
wrongPasswordHandler WrongPassword = do
    liftIO (T.putStrLn "Wrong password, one more chance.")
    userLoginv3
wrongPasswordHandler err =
    throwE err

printError :: LoginError -> EitherIO LoginError a
printError err = do
    liftIO . T.putStrLn $ case err of
        WrongPassword ->
            "Wrong password. No more chances."
        NoSuchUser ->
            "No user with that email exists."
        InvalidEmail ->
            "Invalid email address entered."
    throwE err

loginDialogue :: EitherIO LoginError ()
loginDialogue = do
    let entry = userLoginv3 `catchE` wrongPasswordHandler
    token <- entry `catchE` printError
    liftIO $ T.putStrLn
        (append "Logged in with token: " token)

