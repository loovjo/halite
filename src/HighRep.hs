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
    = HVBind String High
    | HTBind String HType

data TypeContext =
    TypeContext {
        getGenerics :: M.Map String HType,
        getVariables :: M.Map String HType
    }
    deriving (Show, Eq)

emptyTCtx = TypeContext { getGenerics = M.empty, getVariables = M.empty }

data TypeError
    = TypeMismatchError AstCtx HType HType -- Expected, real
    | VarNotFoundError AstCtx String
    deriving (Show)


letters = "abcdefghijklmnopqrstuvqxyz"

varNames :: [String]
varNames =
    tail varNamesWithEmpty
    where
        varNamesWithEmpty =
            "" : ((++) <$> varNamesWithEmpty <*> fmap pure letters)

newVarName :: TypeContext -> String
newVarName tctx = head $ filter (not . flip M.member (getGenerics tctx)) varNames

unifyAT :: TypeContext -> Ast -> HType -> Either TypeError (TypeContext, HType)
unifyAT tctx (RepTree ctx br) ty =
    let simple ty2 =
            case unifyTT tctx ty ty2 of
                Just x -> Right x
                Nothing -> Left $ TypeMismatchError ctx ty2 ty
    in case br of
        ABottom -> simple $ TConstructor "Bottom" []
        AVar name ->
            case M.lookup name $ getVariables tctx of
                Just ty' -> simple ty'
                Nothing -> Left $ VarNotFoundError ctx name
        ANum _ -> simple $ TConstructor "Int" []
        ALet [] ex -> unifyAT tctx ex ty
        ALet (AVBind var expr:rest) ex ->
            case M.lookup var $ getVariables tctx of
                Just vty -> do
                    (tctx', _) <- unifyAT tctx expr vty
                    unifyAT tctx' (RepTree ctx $ ALet rest ex) ty
                Nothing -> do
                    let new = newVarName tctx
                    (tctx', vty) <- unifyAT tctx expr $ TNamed new
                    let tctx'' = tctx' { getVariables = M.insert var vty $ getVariables tctx' }
                    (tctx''', res) <- unifyAT tctx'' (RepTree ctx $ ALet rest ex) ty
                    let rtctx =
                            tctx''' {
                                getVariables = M.delete var $ getVariables tctx''',
                                getGenerics = M.delete new $ getVariables tctx'''
                            }
                    return $ (rtctx, res)
        ALet (ATBind var vty:rest) ex ->
            if M.member var $ getVariables tctx
                then do
                    (tctx', _) <- unifyAT tctx (RepTree ctx $ AVar var) vty
                    unifyAT tctx' (RepTree ctx $ ALet rest ex) ty
                else do
                    let tctx' = tctx { getVariables = M.insert var vty $ getVariables tctx }
                    (tctx'', res) <- unifyAT tctx' (RepTree ctx $ ALet rest ex) ty
                    return $ (tctx'' { getVariables = M.delete var $ getVariables tctx'' }, res)
        ALambda [] ex -> unifyAT tctx ex ty
        ALambda (var:rest) ex ->
            do
                let new = newVarName tctx
                let tctx' = tctx { getVariables = M.insert var (TNamed new) $ getVariables tctx }
                (tctx'', res) <- unifyAT tctx' ex ty

                let varTy =
                        case M.lookup new $ getGenerics tctx'' of
                            Just x -> x
                            Nothing -> TNamed new

                let rtctx =
                        tctx'' {
                            getVariables = M.delete var $ getVariables tctx'',
                            getGenerics = M.delete new $ getVariables tctx''
                        }

                return $ (rtctx, TFunction varTy res)



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

