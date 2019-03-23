module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [file] -> runFile file
        _ -> putStrLn "Please specify one file to run or none to get a repl"
