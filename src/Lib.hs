module Lib where

import Ast
import Parse
import Parser
import HighRep
import HType

import System.IO
import qualified Data.Text as T


showErr i (line:rest) (err, idx) =
    if idx > T.length line
        then showErr (i + 1) rest (err, idx - T.length line - 1)
        else do
            let lineNr = show (i+1) ++ " | "
                indent = take (idx + length lineNr) $ cycle " "
            putStrLn $ lineNr ++ T.unpack line
            putStrLn $ indent ++ "^ " ++ show err
            putStrLn ""

repl :: IO ()
repl =
    putStrLn "Halite Repl" >> loop
    where
        loop = do
            putStr "> "
            hFlush stdout
            inp <- T.pack <$> getLine

            case doParse parseAst inp 0 of
                Right (ast, len) ->
                    if len == T.length inp
                        then do
                            putStrLn $ "Parsed: " ++ apprint 0 ast
                            case unifyAT emptyTCtx ast TUnknown of
                                Right (ctx', ty) ->
                                    putStrLn $ "Type: " ++ show ty ++ ", ctx: " ++ show ctx'
                                Left e ->
                                    putStrLn $ "Type error: " ++ show e

                        else putStrLn "Not all parsed!"
                Left errs ->
                    mapM_ (showErr 0 (T.lines inp)) errs

            loop

runFile :: String -> IO ()
runFile filename = do
    inp <- T.pack <$> readFile filename

    case doParse parseAst inp 0 of
        Right (ast, len) ->
            if len == T.length inp
                then do
                    putStrLn $ "Parsed: " ++ apprint 0 ast
                else putStrLn "Not all parsed!"
        Left errs ->
            mapM_ (showErr 0 (T.lines inp)) errs

