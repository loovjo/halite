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

testUnifyTT :: (M.Map String HType, HType, HType) -> (M.Map String HType, HType) -> IO ()
testUnifyTT (tctx, ty1, ty2) (tctx', ty) =
    do
        putStrLn $
            colBlue ++ show ty1 ++ colReset ++
            "  =~  " ++ colBlue ++ show ty2 ++ colReset ++ " {" ++ show tctx ++ "}"
        case unifyTT tctx ty1 ty2 of
            Just (tctx', res) ->
                putStrLn $ colGreen ++ "Res: " ++ show res ++ ", ctx': " ++ show tctx' ++ colReset
            Nothing ->
                putStrLn $ colRed ++ "Could not unify" ++ colReset


main :: IO ()
main = do
    putStrLn ""

    testUnifyTT
        (M.empty, TConstructor "List" [TConstructor "Int" []], TNamed "a")
        (M.fromList [("a", TConstructor "List" [TConstructor "Int" []])],
            TConstructor "List" [TConstructor "Int" []])

    testUnifyTT
        (M.empty, TConstructor "List" [TNamed "a"], TConstructor "List" [TNamed "b"])
        (M.fromList [("b", TNamed "a")], TConstructor "List" [TNamed "a"])


    testUnifyTT
        (M.empty, TConstructor "List" [TNamed "a"], TConstructor "List" [TConstructor "Int" []])
        (M.fromList [("a", TConstructor "Int" [])], TConstructor "List" [TConstructor "Int" []])

    testUnifyTT
        (M.empty,
            TFunction (TConstructor "Int" []) (TNamed "a"),
            TFunction (TNamed "b") (TConstructor "Int" []))
        (M.fromList [("a", TConstructor "Int" []), ("b",(TConstructor "Int" []))],
            TFunction (TConstructor "Int" []) (TConstructor "Int" []))


