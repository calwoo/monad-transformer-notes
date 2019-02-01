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

