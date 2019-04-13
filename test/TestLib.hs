module TestLib
    ( unitTest
    , unitTestFunc
    , unitTestFuncPred
    ) where

colRed = "\x1b[38;5;1m"
colGreen = "\x1b[38;5;2m"
colBlue = "\x1b[38;5;6m"
colReset = "\x1b[0m"

unitTest :: (Eq a, Show a) => String -> a -> a -> IO ()
unitTest text x y = do
    putStr $ colBlue ++ text ++ "... "
    if x == y
        then putStrLn $ colGreen ++ "Test passed"
        else putStrLn $ colRed ++ "\nGot:    " ++ show x ++ "\nWanted: " ++ show y

unitTestFunc :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> IO ()
unitTestFunc name fn x y = unitTestFuncPred name fn (==) x y

unitTestFuncPred :: (Show a, Show b) => String -> (a -> b) -> (b -> b -> Bool) -> a -> b -> IO ()
unitTestFuncPred name fn pred x y = do
    putStr $ colBlue ++ name ++ " " ++ show x ++ " == " ++ show y ++ "... "
    let y' = fn x
    if pred y' y
        then putStrLn $ colGreen ++ "Test passed"
        else putStrLn $ colRed ++ "\nGot:    " ++ show y' ++ "\nWanted: " ++ show y
