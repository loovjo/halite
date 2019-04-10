module TestTypes (testTypes) where

import qualified Data.Set as S
import qualified Data.Map as M

import TestLib

import Type


testTypes :: IO ()
testTypes = sequence_
    [ testVars
    , testFreeVars
    , testBindFrees
    , testRenameForAlls
    , testLowerForAlls
    ]

testVars = do
    unitTestFunc "vars" vars (TVar "a") (S.singleton "a")
    unitTestFunc "vars" vars (TFunction (TVar "a") (TVar "b")) (S.fromList ["a", "b"])
    unitTestFunc "vars" vars
        (TConstructor "T" [TVar "a", TVar "b", TVar "c"])
        (S.fromList ["a", "b", "c"])

testFreeVars = do
    unitTestFunc "freeVars" freeVars
        (PolyType {inner = TVar "a", forAll=S.empty })
        (S.singleton "a")
    unitTestFunc "freeVars" freeVars
        (PolyType {inner = TVar "a", forAll=S.singleton "a" })
        S.empty
    unitTestFunc "freeVars" freeVars
        (PolyType {inner = TFunction (TVar "a") (TVar "b"), forAll=S.singleton "a" })
        (S.singleton "b")
    unitTestFunc "freeVars" freeVars
        (PolyType {inner = TConstructor "T" [TVar "a", TVar "b"], forAll=S.singleton "b" })
        (S.singleton "a")

testBindFrees = do
    unitTestFunc "bindFrees" bindFrees
        (TConstructor "Int" [])
        (PolyType {
            inner = TConstructor "Int" [],
            forAll = S.empty
        })
    unitTestFunc "bindFrees" bindFrees
        (TConstructor "T" [TFunction (TVar "a") (TVar "b"), TVar "b"])
        (PolyType {
            inner = TConstructor "T" [TFunction (TVar "a") (TVar "b"), TVar "b"],
            forAll = S.fromList ["a", "b"]
        })

testRenameForAlls = do
    unitTestFunc "renameForAlls" (uncurry renameForAlls)
        ( S.fromList ["a", "c"]
        , PolyType {
            inner = TConstructor "Int" [TVar "a", TVar "b", TVar "c", TVar "z"],
            forAll = S.fromList ["a", "b", "c", "z"]
        }
        )
        ( PolyType {
            inner = TConstructor "Int" [TVar "e", TVar "b", TVar "d", TVar "z"],
            forAll = S.fromList ["d", "b", "e", "z"]
        })

testLowerForAlls = do
    unitTestFunc "lowerForAlls" lowerForAlls
        PolyType {
            inner = TConstructor "T" [TVar "x", TVar "hej", TVar "bla", TVar "boi"],
            forAll = S.fromList ["x", "hej", "bla", "boi"]
        }
        ( PolyType {
            inner = TConstructor "T" [TVar "a", TVar "d", TVar "b", TVar "c"],
            forAll = S.fromList ["a", "b", "c", "d"]
        })
