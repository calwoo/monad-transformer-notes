-- I'm fairly comfortable with monads. But I should really memorize the ones people use, in addition to the
-- mathematical definitions I'm most familiar with.

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b 


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
    else $ Just [floor f, ceil f]

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

instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    (Just a) >>= f = f a


