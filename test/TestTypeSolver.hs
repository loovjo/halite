module TestTypeSolver (testTypeSolver) where

import qualified Data.Set as S
import qualified Data.Map as M

import TestLib

import Type
import TypeSolver


testTypeSolver :: IO ()
testTypeSolver = sequence_
    [ testSubstitutable
    , testUnifyMonos
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
