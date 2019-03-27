module HighRep where

import qualified Data.Set as S
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
        getGenerics :: M.Map String GenState,
        getVariables :: M.Map String HType
    }
    deriving (Show, Eq)

data GenState
    = Exists
    | SameAs HType
    deriving (Show, Eq)

emptyTCtx = TypeContext { getGenerics = M.empty, getVariables = M.empty }

data TypeError
    = TypeMismatchError AstCtx HType HType TypeContext -- Expected, real
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

addNewGeneric :: TypeContext -> (TypeContext, String)
addNewGeneric tctx =
    let name = newVarName tctx
        tctx' = tctx { getGenerics = M.insert name Exists $ getGenerics tctx }
    in (tctx', name)

addFrees :: TypeContext -> S.Set String -> TypeContext
addFrees = foldr
        (\var tctx ->
            tctx { getGenerics = M.insert var Exists $ getGenerics tctx }
        )

unifyAT :: TypeContext -> Ast -> HType -> Either TypeError (TypeContext, HType)
unifyAT tctx (RepTree ctx br) ty = -- ty = expected type, return real
    let simple ty2 =
            case unifyTT tctx ty2 ty of
                Just x -> Right x
                Nothing -> Left $ TypeMismatchError ctx ty2 ty tctx
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
                    let (tctx', new) = addNewGeneric tctx
                    (tctx'', vty) <- unifyAT tctx' expr TUnknown
                    let tctx''' = tctx'' { getVariables = M.insert var vty $ getVariables tctx'' }
                    (rtctx, res) <- unifyAT tctx''' (RepTree ctx $ ALet rest ex) ty
                    let rtctx' =
                            rtctx {
                                getVariables = M.delete var $ getVariables rtctx
                            }
                    return $ (rtctx', res)
        ALet (ATBind var vty:rest) ex ->
            let vars = frees vty
                tctx' = addFrees tctx vars
            in if M.member var $ getVariables tctx'
                then do
                    (tctx'', vty') <- unifyAT tctx' (RepTree ctx $ AVar var) vty
                    let tctx''' = tctx'' {
                                getVariables = M.insert var vty $ getVariables tctx''
                            }
                    unifyAT tctx''' (RepTree ctx $ ALet rest ex) ty
                else do
                    let tctx'' = tctx' { getVariables = M.insert var vty $ getVariables tctx' }
                    (tctx''', res) <- unifyAT tctx'' (RepTree ctx $ ALet rest ex) ty
                    return $ (tctx''' { getVariables = M.delete var $ getVariables tctx''' }, res)
        ALambda [] ex -> unifyAT tctx ex ty
        ALambda (var:rest) ex ->
            do
                let (vtctx, new) = addNewGeneric tctx
                let tctx' = vtctx { getVariables = M.insert var (TNamed new) $ getVariables vtctx }

                (tctx'', res) <- unifyAT tctx' (RepTree ctx $ ALambda rest ex) TUnknown
                (tctx''', varTy) <- unifyAT tctx'' (RepTree ctx $ AVar var) TUnknown

                case unifyTT tctx''' ty (TFunction varTy res) of
                    Just (utctx, resty) ->
                        let rtctx =
                                utctx {
                                    getVariables = M.delete var $ getVariables utctx
                                }
                        in Right (rtctx, resty)
                    Nothing -> Left $ TypeMismatchError ctx (TFunction varTy res) ty tctx
        ACall [ex] -> unifyAT tctx ex ty
        ACall args ->
            let f = RepTree ctx $ ACall $ init args
                x = last args
            in do
                (tctx', xty) <- unifyAT tctx x TUnknown
                (tctx'', fty) <- unifyAT tctx' f (TFunction xty ty)
                case fty of
                    TFunction _ res -> Right (tctx'', res)
                    _ -> Left $ TypeMismatchError ctx fty (TFunction xty ty) tctx''


unifyTT :: TypeContext -> HType -> HType -> Maybe (TypeContext, HType)
unifyTT tctx t1 t2 = -- t1 is real type, t2 is expected
    case (t1, t2) of
        (TUnknown, _) -> Just (tctx, t2)
        (_, TUnknown) -> Just (tctx, t1)
        (TNamed a, TNamed b) | a == b -> Just (tctx, t1)
        (TNamed a, _) ->
            case M.lookup a $ getGenerics tctx of
                Just (SameAs t1') -> unifyTT tctx t1' t2
                Just Exists ->
                    let tctx' = tctx { getGenerics = M.insert a (SameAs t2) $ getGenerics tctx }
                    in Just (tctx', t2)
                _ -> Nothing
        (_, TNamed b) ->
            case M.lookup b $ getGenerics tctx of
                Just (SameAs t2') -> unifyTT tctx t1 t2'
                Just Exists ->
                    let tctx' = tctx { getGenerics = M.insert b (SameAs t1) $ getGenerics tctx }
                    in Just (tctx', t1)
                _ -> Nothing
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

