import Control.Monad.Reader


-- READER MONAD -- these give us a notion of global environment

-- in Haskell, code is pure, which means functions can only interact with arguments passed into them.
-- this precludes global variables. 

-- example) suppose we want to have a global environment loaded in via IO

data Environment = Environment
    {   param1 :: String,
        param2 :: String,
        param3 :: String }

loadEnv :: IO Environment
loadEnv = return env
    where env = Environment "ha" "what" "circle"

main :: IO ()
main = do
    env <- loadEnv
    let str = func1 env
    putStrLn str

func1 :: Environment -> String
func1 env = "Result: " ++ (show (func2 env))

func2 :: Environment -> Int
func2 env = 2 + (length $ param2 env)

-- note that only func2 is really using the environment, func1 is just passing it along. but this can get
-- cumbersome, as func2 can't directly call loadEnv because it's a pure function and loadEnv is impure.

-- we want to have global read-only values. the reader monad fixes this for us.

func1' :: Reader Environment String
func1' = do
    res <- func2
    return ("Result: " ++ (show res))

func2' :: Reader Environment Int
func2' = do
    env <- ask -- this is Reader's internal loadEnv
    let res2 = param2 env
    return (2 + (length $ res2))

main' :: IO ()
main' = do
    env <- loadEnv
    let str = runReader func1' env
    putStrLn str