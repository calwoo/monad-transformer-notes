-- I'm fairly comfortable with monads. But I should really memorize the ones people use, in addition to the
-- mathematical definitions I'm most familiar with.

import Data.Char

{- class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b  -}

-- MAYBE MONAD

maybe1 :: String -> Maybe Int
maybe1 "" = Nothing
maybe1 str = Just $ length str

maybe2 :: Int -> Maybe Float
maybe2 i = if i `mod` 2 == 0
    then Nothing
    else Just $ (fromIntegral i) * 3.14159

maybe3 :: Float -> Maybe [Int]
maybe3 f = if f > 15.0
    then Nothing
    else Just [floor f, ceiling f]

-- okay, compose them?
maybeComposed :: String -> Maybe [Int]
maybeComposed input = case maybe1 input of
    Nothing -> Nothing
    Just i -> case maybe2 i of
        Nothing -> Nothing
        Just f -> maybe3 f
-- barf! this is ugly. if we have more functions, the composition chain might be horrible.
-- but we sorta know what's going on-- maybe <blah> is a computational context, and each time we are extracting
-- the data from the context and doing another computation to it. we need a way to chain these.
-- that's a monad!

{- instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    (Just a) >>= f = f a -}

maybeComposedv2 :: String -> Maybe [Int]
maybeComposedv2 input = maybe1 input >>= maybe2 >>= maybe3

maybeComposedv3 :: String -> Maybe [Int]
maybeComposedv3 input = do
    i <- maybe1 input
    f <- maybe2 i 
    maybe3 f 

-- the do notation unwraps as just the syntactical sugar for monadic composition
maybeComposedv3undo :: String -> Maybe [Int]
maybeComposedv3undo input =
    maybe1 input
    >>= (\i -> maybe2 i)
    >>= (\f -> maybe3 f)



-- EITHER MONAD -- is like the maybe monad, but instead of just giving failure, it gives information with failure
{- instance Monad (Either a) where
    return r = Right r
    (Left l) >>= _ = Left l -- short circuits on failure
    (Right r) >>= f = f r -- continues computation -}

either1 :: String -> Either String Int -- note that the monad is (Either String), where String is the error type
either1 "" = Left "String can't be empty"
either1 str = Right $ length str

either2 :: Int -> Either String Float
either2 i = if i `mod` 2 == 0
    then Left "Length can't be even"
    else Right $ (fromIntegral i) * 3.14159

either3 :: Float -> Either String [Int]
either3 f = if f > 15.0
    then Left "Float is greater than 15"
    else Right [floor f, ceiling f]

eitherComposed :: String -> Either String [Int]
eitherComposed input = do
    i <- either1 input
    f <- either2 i
    either3 f


-- IO MONAD -- this is a wrapper for all computations that perform side effects
-- as an example

main :: IO ()
main = do
    -- getLine :: IO String
    input <- getLine
    let uppercased = map Data.Char.toUpper input
    -- print :: String -> IO ()
    print uppercased





