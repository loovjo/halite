module Ast where

import RepTree
import HType

import Data.List


type Ast = RepTree AstCtx AstBranch

data AstBranch
    = ABottom
    | AVar String
    | ANum Int
    | AConstructor String
    | ALet [ABind] Ast
    | ALambda [String] Ast
    | ACall [Ast]

data AstCtx =
    AstCtx { cspan :: (Int, Int) }
    deriving Show

data ABind
    = AVBind String Ast
    | ATBind String HType

indentOf :: Int -> String
indentOf x = take (x * 4) $ cycle " "

apprint :: Int -> Ast -> String
apprint i (RepTree ctx part) =
    appprint part
    where
        indent = indentOf i
        appprint part =
            case part of
                ABottom -> "!"
                AVar x -> x
                ANum x -> show x
                AConstructor x -> x
                ALet binds body ->
                    let sep = "\n" ++ indentOf (i + 1)
                        ppbinds = map (ppbind i) binds
                        ppbinds' = intercalate sep ppbinds
                        ppbody = apprint (i + 1) body
                    in "let " ++ ppbinds' ++ "\n" ++ indent ++ "in " ++ ppbody
                ALambda vars body ->
                    let ppvars = intercalate " " vars
                        ppbody = apprint (i + 1) body
                    in "λ" ++ ppvars ++ ". " ++ ppbody
                ACall fs ->
                    let ppfs = concatMap (\v -> apprint (i + 1) v ++ " ") fs
                    in "(" ++ (take (length ppfs - 1) ppfs) ++ ")"

ppbind :: Int -> ABind -> String
ppbind i bind =
    case bind of
        AVBind v e -> v ++ " = " ++ apprint (i + 2) e ++ ";"
        ATBind v t -> v ++ " :: " ++ ppType t
