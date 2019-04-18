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
                [ ("a", TVar "c")
                , ("e", (TConstructor "List" [TVar "c"]))
                ]
        )

gtTest :: T.Text -> PolyType -> IO ()
gtTest code wanted = do
    case doParse parseAst code 0 of
        Right (ast, r) | r == T.length code ->
            let dst = ast2dst ast
                t1 = lowerForAlls <$> snd <$> getType defaultContext dst
            in unitTest ("`" ++ T.unpack code ++ "` has type " ++ show wanted)
                    t1 (Right $ lowerForAlls wanted)
        Left e -> do
            putStrLn "\x1b[48;5;1mCouldn't parse input!"
            putStrLn $ "> " ++ T.unpack code
            putStrLn $ show e
        _ -> do
            putStrLn "\x1b[48;5;1mCouldn't parse input!"
            putStrLn $ "> " ++ T.unpack code

testGetType = do
    gtTest "5" (bindFrees $ TConstructor "Int" [])
    gtTest "5+7" (bindFrees $ TConstructor "Int" [])
    gtTest "\\x. cons x empty" (bindFrees $ TFunction (TVar "a") (TConstructor "List" [TVar "a"]) )
    gtTest "\\f x. f x"
        (bindFrees $
            TFunction
                (TFunction (TVar "a") (TVar "b"))
                (TFunction (TVar "a") (TVar "b"))
        )
    gtTest "map(\\x.empty)"
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

    gtTest "\\l.map(\\a.a+1)l"
        ( bindFrees $
            TFunction
                (TConstructor "List" [TConstructor "Int" []])
                (TConstructor "List" [TConstructor "Int" []])
        )

    gtTest "\\l.map(cons 1)l"
        ( bindFrees $
            TFunction
                (TConstructor "List" [TConstructor "List" [TConstructor "Int" []]])
                (TConstructor "List" [TConstructor "List" [TConstructor "Int" []]])
        )

    gtTest "let combine::(b->c)->(a->b)->a->c in combine (\\a.a+1) (\\a.a*2)"
        ( bindFrees $
            TFunction
                (TConstructor "Int" [])
                (TConstructor "Int" [])
        )

    gtTest "let combine::(b->c)->(a->b)->a->c in combine (map (\\a.a+1)) (map (\\a.a+1))"
        ( bindFrees $
            TFunction
                (TConstructor "List" [TConstructor "Int" []])
                (TConstructor "List" [TConstructor "Int" []])
        )
