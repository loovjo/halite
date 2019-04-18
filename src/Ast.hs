module Ast where

import RepTree
import Type

import Data.List

type Ast = RepTree AstCtx AstBranch

data AstBranch
    = AVar String
    | ANum Int
    | AConstructor String
    | ALet [ABind] Ast
    | ALambda [String] Ast
    | ACall [Ast]

instance Show AstBranch where
    show = apprint 0 . RepTree (AstCtx {cspan=(-1,-1)})

data AstCtx =
    AstCtx { cspan :: (Int, Int) }
    deriving Show

data ABind
    = AVBind String Ast
    | ATBind String PolyType

indentOf :: Int -> String
indentOf x = take (x * 4) $ cycle " "

apprint :: Int -> Ast -> String
apprint i (RepTree _ part) =
    appprint part
    where
        indent = indentOf i
        appprint part =
            case part of
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
                    in "(λ" ++ ppvars ++ ". " ++ ppbody ++ ")"
                ACall fs ->
                    let ppfs = concatMap (\v -> apprint (i + 1) v ++ " ") fs
                    in "(" ++ (take (length ppfs - 1) ppfs) ++ ")"

ppbind :: Int -> ABind -> String
ppbind i bind =
    case bind of
        AVBind v e -> v ++ " = " ++ apprint (i + 2) e ++ ";"
        ATBind v t -> v ++ " :: " ++ show t
