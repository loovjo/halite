module Dst where

-- Dst is like an Ast but desugared.

import Ast
import RepTree
import Type

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
