module HighRep where

import qualified Data.Map.Strict as M
import RepTree
import HType
import Ast

import Data.List


type Program = [HighBind]

type High = RepTree HighCtx HighBranch

data HighBranch
    = HBottom
    | HVar String
    | HNum Int
    | HConstructor String
    | HLet String High High
    | HLambda [String] High
    | HCall [High]

data HighCtx =
    HighCtx HType
    deriving Show

data HighBind
    = AVBind String High
    | ATBind String HType

type TypeContext = M.Map String HType

data TypeError
    = TypeMismatchError (HType, AstCtx) HType




-- unifyAT :: TypeContext -> Ast -> HType -> Either TypeError (TypeContext, HType)
-- unifyAT tctx (RepTree actx ast) ty =
--     let unifyTTWithCtx wanted =
--             case unifyTT tctx ty wanted of
--                 Just x -> Right x
--                 Nothing -> Left $ TypeMismatchError (ty, actx) wanted
--     case ast of
--         ABottom -> unifyTTWithCtx $ TConstructor "Bottom" []
--         AVar -> unifyTTWithCtx $ TConstructor "Bottom" []

unifyTT :: TypeContext -> HType -> HType -> Maybe (TypeContext, HType)
unifyTT tctx t1 t2 =
    case (t1, t2) of
        (TNamed a, TNamed b) | a == b -> Just (tctx, t1)
        (TNamed a, TNamed b) ->
            case (M.lookup a tctx, M.lookup b tctx) of
                (Just t1', Just t2') -> unifyTT tctx t1' t2'
                (Just t1', Nothing) ->
                    let tctx' = M.insert b t1' tctx
                    in Just (tctx', t1')
                (Nothing, Just t2') ->
                    let tctx' = M.insert b t2' tctx
                    in Just (tctx', t2')
                (Nothing, Nothing) ->
                    let tctx' = M.insert b t1 tctx
                    in Just (tctx', t1)
        (TNamed a, _) ->
            case M.lookup a tctx of
                Just t1' -> unifyTT tctx t1' t2
                Nothing ->
                    let tctx' = M.insert a t2 tctx
                    in Just (tctx', t2)
        (_, TNamed b) ->
            case M.lookup b tctx of
                Just t1' -> unifyTT tctx t1' t2
                Nothing ->
                    let tctx' = M.insert b t1 tctx
                    in Just (tctx', t1)
        (TConstructor acons aargs, TConstructor bcons bargs)
            | acons == bcons && length aargs == length bargs ->
                let res =
                        foldl' (\mctx (aarg, barg) ->
                            case mctx of
                                Nothing -> Nothing
                                Just (tctx, args) ->
                                    case unifyTT tctx aarg barg of
                                        Just (tctx', arg) -> Just (tctx', arg:args)
                        ) (Just (tctx, [])) $ zip aargs bargs
                in case res of
                    Just (tctx', args) -> Just (tctx', TConstructor acons args)
        (TFunction af ax, TFunction bf bx) ->
            do
                (tctx', f) <- unifyTT tctx af bf
                (tctx'', x) <- unifyTT tctx' ax bx
                return $ (tctx'', TFunction f x)
        _ -> Nothing

