module Dst where

-- Dst is like an Ast but desugared.

import qualified Data.Map.Strict as M
import Data.List

import Ast
import RepTree
import Type

data Assoc
    = LtR
    | RtL

operatorPrecedence :: M.Map String Int
operatorPrecedence =
    M.fromList
        [ ("^", 0)
        , ("*", 1)
        , ("/", 1)
        , ("%", 1)
        , ("+", 2)
        , ("-", 2)
        , (".", 3)
        , ("$", 4)
        ]

precedenceAssoc :: [Assoc]
precedenceAssoc =
    [ RtL -- 0
    , LtR -- 1
    , LtR -- 2
    , RtL -- 3
    , RtL -- 4
    ]


type Dst = RepTree DstCtx DstBranch

data DstBranch
    = DVar String
    | DNum Int
    | DConstructor String
    | DLet [(String, PolyType)] [(String, Dst)] Dst
    | DLambda String Dst
    | DCall Dst Dst
    deriving Show

data DstCtx =
    DstCtx { innerAstCtx :: AstCtx }
    deriving Show

dpprint :: Dst -> String
dpprint (RepTree _ (DVar x)) = x
dpprint (RepTree _ (DNum x)) = show x
dpprint (RepTree _ (DConstructor x)) = x
dpprint (RepTree _ (DLet ts vs exp)) =
    "let " ++ intercalate "; " (map (\(x, t) -> x ++ "::" ++ show t) ts)
        ++ intercalate "; " (map (\(x, t) -> x ++ " = " ++ dpprint t) vs)
        ++ " in " ++ dpprint exp
dpprint (RepTree _ (DLambda var exp)) = "\\" ++ var ++ ". " ++ dpprint exp
dpprint (RepTree _ (DCall f x)) = "(" ++ dpprint f ++ ") (" ++ dpprint x ++ ")"


ast2dst :: Ast -> Dst
ast2dst (RepTree ctx (AVar x))         = RepTree (DstCtx { innerAstCtx = ctx }) $ DVar x
ast2dst (RepTree ctx (ANum x))         = RepTree (DstCtx { innerAstCtx = ctx }) $ DNum x
ast2dst (RepTree ctx (AConstructor x)) = RepTree (DstCtx { innerAstCtx = ctx }) $ DConstructor x
ast2dst (RepTree ctx (ALet lets exp)) =
    let (tbinds, vbinds) =
            foldr (\bind (tbinds, vbinds) ->
                case bind of
                    AVBind name exp -> (tbinds, (name, ast2dst exp) : vbinds)
                    ATBind name ty -> ((name, ty) : tbinds, vbinds)
            ) ([], []) lets
    in RepTree (DstCtx { innerAstCtx = ctx }) $ DLet tbinds vbinds $ ast2dst exp
ast2dst (RepTree _ (ALambda [] exp)) = ast2dst exp
ast2dst (RepTree ctx (ALambda (var:rest) exp)) =
    let exp' = RepTree ctx (ALambda rest exp)
        dexp = ast2dst exp'
    in RepTree (DstCtx { innerAstCtx = ctx }) $
        DLambda var dexp
ast2dst (RepTree _ (ACall [exp])) = ast2dst exp
ast2dst (RepTree ctx (ACall call)) =
    let f = RepTree ctx $ ACall $ init call
        x = last call
        df = ast2dst f
        dx = ast2dst x
    in RepTree (DstCtx { innerAstCtx = ctx }) $ DCall df dx

ast2dst (RepTree ctx (ABinOps first [])) = ast2dst first
ast2dst (RepTree ctx (ABinOps first ops)) =
    let highestPred = maximum $
            map
                (\(RepTree _ (BNamed op), _) ->
                    operatorPrecedence M.! op
                )
                ops
        opsWithIdx = zip [0..] ops
        cmp =
            case precedenceAssoc !! highestPred of
                LtR -> (>=)
                RtL -> (>)
        (opIdx, _) =
            foldl
                (\(bidx, bpred) (oidx, (RepTree _ (BNamed op), _)) ->
                    let opred = operatorPrecedence M.! op
                    in if opred `cmp` bpred
                        then (oidx, opred)
                        else (bidx, bpred)
                ) (-1, -1)
                opsWithIdx
        until = take opIdx ops
        after = drop (opIdx+1) ops
        (RepTree octx (BNamed op), afterFirst) = ops !! opIdx
        dUntil = ast2dst (RepTree ctx (ABinOps first until))
        dAfter = ast2dst (RepTree ctx (ABinOps afterFirst after))
    in
        RepTree (DstCtx { innerAstCtx = ctx }) (
            DCall (RepTree (DstCtx { innerAstCtx = ctx }) $
                DCall (RepTree (DstCtx { innerAstCtx = octx }) $ DVar op) dUntil
            ) dAfter
        )
