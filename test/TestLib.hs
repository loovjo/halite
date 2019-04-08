module TestLib
    ( unitTest
    , unitTestFunc
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
unitTestFunc name fn x y = do
    putStr $ colBlue ++ name ++ " " ++ show x ++ " == " ++ show y ++ "... "
    let y' = fn x
    if y' == y
        then putStrLn $ colGreen ++ "Test passed"
        else putStrLn $ colRed ++ "\nGot:    " ++ show y' ++ "\nWanted: " ++ show y
