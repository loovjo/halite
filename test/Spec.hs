{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Map as M
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
        else putStrLn $ colRed ++ show x ++ " is not " ++ show y

main :: IO ()
main = foldl1 (>>)
    [ putStrLn ""
    , testUnifyTT
    , testNewVarName
    ]

g2t generics = TypeContext { getGenerics = generics, getVariables = M.empty}

testUnifyTTUtil :: (M.Map String HType, HType, HType) -> (M.Map String HType, HType) -> IO ()
testUnifyTTUtil (generics, ty1, ty2) (generics', ty) =
    let tctx = g2t generics
        tctx' = g2t generics'
    in unitTest (show ty1 ++ " <> " ++ show ty2 ++ " {" ++ show tctx ++ "}")
        (unifyTT tctx ty1 ty2)
        (Just (tctx', ty))

testUnifyTT :: IO ()
testUnifyTT = do
    testUnifyTTUtil
        (M.empty, TConstructor "List" [TConstructor "Int" []], TNamed "a")
        (M.fromList [("a", TConstructor "List" [TConstructor "Int" []])],
            TConstructor "List" [TConstructor "Int" []])

    testUnifyTTUtil
        (M.empty, TConstructor "List" [TNamed "a"], TConstructor "List" [TNamed "b"])
        (M.fromList [("b", TNamed "a")], TConstructor "List" [TNamed "a"])


    testUnifyTTUtil
        (M.empty, TConstructor "List" [TNamed "a"], TConstructor "List" [TConstructor "Int" []])
        (M.fromList [("a", TConstructor "Int" [])], TConstructor "List" [TConstructor "Int" []])

    testUnifyTTUtil
        (M.empty,
            TFunction (TConstructor "Int" []) (TNamed "a"),
            TFunction (TNamed "b") (TConstructor "Int" []))
        (M.fromList [("a", TConstructor "Int" []), ("b",(TConstructor "Int" []))],
            TFunction (TConstructor "Int" []) (TConstructor "Int" []))

testNewVarName :: IO ()
testNewVarName = do
    unitTest "NewVarName empty" (newVarName $ g2t M.empty) "a"

    unitTest "NewVarName some"
        (newVarName $ g2t $ M.fromList [("a", undefined), ("b", undefined)]) "c"
    unitTest "NewVarName many"
        (newVarName $ g2t $ M.fromList $ fmap (\a -> (a, undefined)) $ take 200 varNames) "gs"
