{-# LANGUAGE OverloadedStrings #-}

module TestTypeSolver (testTypeSolver) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import Control.Arrow

import TestLib

import Ast
import Dst
import Parse
import Parser
import Type
import TypeSolver


testTypeSolver :: IO ()
testTypeSolver = sequence_
    [ testSubstitutable
    , testUnifyMonos
    , testGetType
    ]

testSubstitutable = do
    unitTestFunc "compose" (uncurry compose)
        ( Substitution $ M.singleton
            "a" (TConstructor "Int" [])
        , Substitution $ M.singleton
            "b" (TConstructor "List" [TConstructor "Char" []])
        )
        ( Right $ Substitution $ M.fromList
            [ ("a", (TConstructor "Int" []))
            , ("b", (TConstructor "List" [TConstructor "Char" []]))
            ]
        )
    unitTestFunc "compose" (uncurry compose)
        ( Substitution $ M.singleton
            "a" (TConstructor "Int" [])
        , Substitution $ M.singleton
            "a" (TVar "b")
        )
        ( Right $ Substitution $ M.fromList
            [ ("a", (TConstructor "Int" []))
            , ("b", (TConstructor "Int" []))
            ]
        )

testUnifyMonos = do
    unitTestFunc "unifyMonos" (uncurry unifyMonos)
        ( TVar "a"
        , TConstructor "Int" [] )
        ( Right $ Substitution $ M.singleton "a" $ TConstructor "Int" [] )

    unitTestFunc "unifyMonos" (uncurry unifyMonos)
        ( TFunction (TVar "a") (TConstructor "Int" [])
        , TFunction (TConstructor "Char" []) (TVar "b")
        )
        ( Right $ Substitution $
            M.fromList
                [ ("a", TConstructor "Char" [])
                , ("b", TConstructor "Int" [])
                ]
        )

    unitTestFunc "unifyMonos" (uncurry unifyMonos)
        ( TVar "a"
        , TConstructor "List" [ TVar "a" ] )
        ( Left $ InfiniteType "a" $ TConstructor "List" [ TVar "a" ] )

    unitTestFunc "unifyMonos" (uncurry unifyMonos)
        ( TFunction
            (TConstructor "List" [TVar "c"])
            (TConstructor "List" [TVar "c"])
        ,  TFunction
            (TConstructor "List" [TVar "a"])
            (TVar "e")
        )
        ( Right $ Substitution $
            M.fromList
                [ ("c", TVar "a")
                , ("e", (TConstructor "List" [TVar "c"]))
                ]
        )

gtTest :: T.Text -> PolyType -> IO ()
gtTest code wanted = do
    case doParse parseAst code 0 of
        Left e -> do
            putStrLn "\x1b[48;5;1mCouldn't parse input!"
            putStrLn $ "> " ++ T.unpack code
            putStrLn $ show e
        Right (ast, _) ->
            let dst = ast2dst ast
                t1 = getType defaultContext dst
            in unitTest ("`" ++ T.unpack code ++ "` has type " ++ show wanted)
                    ((second lowerForAlls) <$> t1) (Right (emptySub, (lowerForAlls wanted)))

testGetType = do
    gtTest "5" (bindFrees $ TConstructor "Int" [])
    gtTest "add 5 7" (bindFrees $ TConstructor "Int" [])
    gtTest "\\x. cons x empty" (bindFrees $ TFunction (TVar "a") (TConstructor "List" [TVar "a"]) )
    gtTest "\\f x. f x"
        (bindFrees $
            TFunction
                (TFunction (TVar "a") (TVar "b"))
                (TFunction (TVar "a") (TVar "b"))
        )
    gtTest "map (\\x. empty)"
        ( bindFrees $
            TFunction
                (TConstructor "List" [TVar "a"])
                (TConstructor "List" [TConstructor "List" [TVar "b"]])
        )

    gtTest "let head :: List a -> a in \\lst. map head lst"
        ( bindFrees $
            TFunction
                (TConstructor "List" [TConstructor "List" [TVar "a"]])
                (TConstructor "List" [TVar "a"])
        )

    gtTest "\\lst. map inc lst"
        ( bindFrees $
            TFunction
                (TConstructor "List" [TConstructor "Int" []])
                (TConstructor "List" [TConstructor "Int" []])
        )

    gtTest "\\lst. map (cons 1) lst"
        ( bindFrees $
            TFunction
                (TConstructor "List" [TConstructor "List" [TConstructor "Int" []]])
                (TConstructor "List" [TConstructor "List" [TConstructor "Int" []]])
        )
