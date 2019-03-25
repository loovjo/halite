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

data TypeContext =
    TypeContext {
        getGenerics :: M.Map String HType,
        getVariables :: M.Map String HType
    }
    deriving (Show, Eq)

data TypeError
    = TypeMismatchError (HType, AstCtx) HType


letters = "abcdefghijklmnopqrstuvqxyz"

varNames :: [String]
varNames =
    tail varNamesWithEmpty
    where
        varNamesWithEmpty =
            "" : ((++) <$> varNamesWithEmpty <*> fmap pure letters)

newVarName :: TypeContext -> String
newVarName tctx = head $ filter (not . flip M.member (getGenerics tctx)) varNames


unifyTT :: TypeContext -> HType -> HType -> Maybe (TypeContext, HType)
unifyTT tctx t1 t2 =
    case (t1, t2) of
        (TNamed a, TNamed b) | a == b -> Just (tctx, t1)
        (TNamed a, TNamed b) ->
            case (M.lookup a $ getGenerics tctx, M.lookup b $ getGenerics tctx) of
                (Just t1', Just t2') -> unifyTT tctx t1' t2'
                (Just t1', Nothing) ->
                    let tctx' = tctx { getGenerics = M.insert b t1' $ getGenerics tctx }
                    in Just (tctx', t1')
                (Nothing, Just t2') ->
                    let tctx' = tctx { getGenerics = M.insert b t2' $ getGenerics tctx }
                    in Just (tctx', t2')
                (Nothing, Nothing) ->
                    let tctx' = tctx { getGenerics = M.insert b t1 $ getGenerics tctx }
                    in Just (tctx', t1)
        (TNamed a, _) ->
            case M.lookup a $ getGenerics tctx of
                Just t1' -> unifyTT tctx t1' t2
                Nothing ->
                    let tctx' = tctx { getGenerics = M.insert a t2 $ getGenerics tctx }
                    in Just (tctx', t2)
        (_, TNamed b) ->
            case M.lookup b $ getGenerics tctx of
                Just t1' -> unifyTT tctx t1' t2
                Nothing ->
                    let tctx' = tctx { getGenerics = M.insert b t1 $ getGenerics tctx }
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

