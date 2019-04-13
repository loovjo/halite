{-# LANGUAGE OverloadedStrings #-}

module TestTypeSolver (testTypeSolver) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import TestLib

import Ast
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

tyEq (Right (s1, t1)) (Right (s2, t2)) =
    let tMatch = case unifyMonos (inner t1) (inner t2) of
                Left _ -> False
                Right sub ->
                    let nonVars = M.filter (\v ->
                            case v of
                                TVar _ -> False
                                _ -> True
                            ) (subMap sub)
                    in M.size nonVars == 0
    in s1 == s2 && tMatch
tyEq _ _ = False

gtTest :: T.Text -> PolyType -> IO ()
gtTest code wanted = do
    case doParse parseAst code 0 of
        Left e -> do
            putStrLn "\x1b[48;5;1mCouldn't parse input!"
            putStrLn $ "> " ++ T.unpack code
            putStrLn $ show e
        Right (ast, _) ->
            unitTestFuncPred "getType" (getType defaultContext) tyEq
                ast (Right (emptySub, wanted))

testGetType = do
    gtTest "5" (bindFrees $ TConstructor "Int" [])
    gtTest "add 5 7" (bindFrees $ TConstructor "Int" [])
    gtTest "\\x. cons x empty" (bindFrees $ TFunction (TVar "a") (TConstructor "List" [TVar "a"]) )
    gtTest "map (\\x. empty)"
        ( bindFrees $
            TFunction (TConstructor "List" [TVar "a"])
                (TConstructor "List" [TConstructor "List" [TVar "b"]])
        )
