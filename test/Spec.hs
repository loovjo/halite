{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Ast
import HType
import HighRep
import RepTree

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

main :: IO ()
main = foldl1 (>>)
    [ putStrLn ""
    , testUnifyTT
    , testFrees
    , testNewVarName
    ]

g2t generics = TypeContext { getGenerics = generics, getVariables = M.empty}

testFrees :: IO ()
testFrees = do
    unitTest "Frees none"
        (frees $ TConstructor "Map" [TConstructor "Int" [], TConstructor "List" [TConstructor "Char" []]])
        S.empty
    unitTest "Frees one"
        (frees $ TConstructor "Map" [TNamed "a", TConstructor "List" [TConstructor "Char" []]])
        $ S.singleton "a"
    unitTest "Frees many"
        (frees $ TConstructor "Map" [TNamed "a", TConstructor "Either" [TNamed "x", TNamed "y"]])
        $ S.fromList ["a", "x", "y"]

    unitTest "Add frees"
        (addFrees emptyTCtx $ S.fromList ["a", "x", "y"])
        (g2t $ M.fromList [("a", Exists), ("x", Exists), ("y", Exists)])


testUnifyTTUtil :: (M.Map String GenState, HType, HType) -> (M.Map String GenState, HType) -> IO ()
testUnifyTTUtil (generics, ty1, ty2) (generics', ty) =
    let tctx = g2t generics
        tctx' = g2t generics'
    in unitTest (show ty1 ++ " <> " ++ show ty2 ++ " {" ++ show tctx ++ "}")
        (unifyTT tctx ty1 ty2)
        (Just (tctx', ty))

testUnifyTT :: IO ()
testUnifyTT = do
    testUnifyTTUtil
        (M.fromList [("a", Exists)], TConstructor "List" [TConstructor "Int" []], TNamed "a")
        (M.fromList [("a", SameAs $ TConstructor "List" [TConstructor "Int" []])],
            TConstructor "List" [TConstructor "Int" []])

    testUnifyTTUtil
        (M.fromList [("a", Exists), ("b", Exists)],
            TConstructor "List" [TNamed "a"],
            TConstructor "List" [TNamed "b"])
        (M.fromList [("a", SameAs $ TNamed "b"), ("b", Exists)],
            TConstructor "List" [TNamed "b"])


    testUnifyTTUtil
        (M.fromList [("a", Exists)],
            TConstructor "List" [TNamed "a"],
            TConstructor "List" [TConstructor "Int" []])
        (M.fromList [("a", SameAs $ TConstructor "Int" [])],
            TConstructor "List" [TConstructor "Int" []])

    testUnifyTTUtil
        (M.fromList [("a", Exists), ("b", Exists)],
            TFunction (TConstructor "Int" []) (TNamed "a"),
            TFunction (TNamed "b") (TConstructor "Int" []))
        (M.fromList [("a", SameAs $ TConstructor "Int" []), ("b", (SameAs $ TConstructor "Int" []))],
            TFunction (TConstructor "Int" []) (TConstructor "Int" []))

    testUnifyTTUtil
        (M.fromList [("a", Exists)],
            TFunction (TNamed "a") (TNamed "a"),
            TFunction (TConstructor "Int" []) (TConstructor "Int" []))
        (M.fromList [("a", SameAs $ TConstructor "Int" [])],
            TFunction (TConstructor "Int" []) (TConstructor "Int" []))

    testUnifyTTUtil
        (M.fromList [("a", Exists), ("b", Exists)],
            TFunction (TNamed "a") (TFunction (TNamed "b") (TNamed "a")),
            TFunction (TConstructor "Int" []) (TFunction (TConstructor "Int" []) (TConstructor "Int" [])))
        (M.fromList [("a", SameAs $ TConstructor "Int" []), ("b", SameAs $ TConstructor "Int" [])],
            TFunction (TConstructor "Int" []) (TFunction (TConstructor "Int" []) (TConstructor "Int" [])))

testNewVarName :: IO ()
testNewVarName = do
    unitTest "NewVarName empty" (newVarName $ g2t M.empty) "a"

    unitTest "NewVarName some"
        (newVarName $ g2t $ M.fromList [("a", undefined), ("b", undefined)]) "c"
    unitTest "NewVarName many"
        (newVarName $ g2t $ M.fromList $ fmap (\a -> (a, undefined)) $ take 200 varNames) "gs"
