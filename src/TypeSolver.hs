{-# LANGUAGE TupleSections #-}

module TypeSolver where

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Except

import Debug.Trace

import Type
import Ast
import RepTree

data Substitution =
    Substitution { subMap :: M.Map String MonoType }
    deriving Eq

instance Show Substitution where
    show sub =
        "[" ++
            intercalate "," (map (\(v, t) -> v ++ "/" ++ show t) $ M.toList $ subMap sub)
        ++ "]"

data TypeSolverState =
    TypeSolverState {
        currentVarID :: Int
    }
    deriving (Show)

initTypeSolverState = TypeSolverState { currentVarID = 0 }

freshVar :: ExceptT e (State TypeSolverState) String
freshVar = do
    current <- currentVarID <$> get
    modify (\tss -> tss { currentVarID = succ current })
    return (varNames !! current)

emptySub = Substitution M.empty

compose :: Substitution -> Substitution -> Either TypeError Substitution
compose s1 s2 =
    let inter = M.intersectionWith (,) (subMap s1) (subMap s2)
    in if M.size inter == 0
        then Right $ Substitution $ M.union (subMap s1) (subMap s2)
        else
            let firstName = (M.keys inter !! 0)
                (t1, t2) = inter M.! firstName
            in do
                sub <- unifyMonos t1 t2
                let t = applySub sub t1
                    s1' = Substitution $ M.insert firstName t (subMap s1)
                    s2' = Substitution $ M.delete firstName (subMap s2)

                s <- compose s1' s2'
                compose s sub


class Substitutable a where
    applySub :: Substitution -> a -> a

instance Substitutable MonoType where
    applySub sub (TVar x) =
        case M.lookup x $ subMap sub of
            Just a -> applySub sub a
            Nothing -> TVar x
    applySub sub (TConstructor name params) =
        TConstructor name $ map (applySub sub) params
    applySub sub (TFunction f x) =
        TFunction (applySub sub f) (applySub sub x)

instance Substitutable PolyType where
    applySub sub ty =
        ty {
            inner = applySub (Substitution $ M.withoutKeys (subMap sub) (forAll ty)) $ inner ty
        }

data TypeContext =
    TypeContext {
        varTypes :: M.Map String PolyType
    }
    deriving (Show)

instance Substitutable TypeContext where
    applySub sub tctx = tctx { varTypes = M.map (mapInner (applySub sub)) $ varTypes tctx }

insertVar :: String -> PolyType -> TypeContext -> TypeContext
insertVar name ty tctx =
    let frees = S.fromList (concatMap (S.toList . freeVars . snd) $ M.toList $ varTypes tctx)
    in if 0 == S.size ( freeVars ty `S.intersection` frees )
        then tctx { varTypes = M.insert name ty $ varTypes tctx }
        else error ("Trying to insert " ++ name ++ "=" ++ show ty ++ " in " ++ show tctx)


unifyMonos :: MonoType -> MonoType -> Either TypeError Substitution
unifyMonos (TVar a) x =
    if S.member a $ vars x
        then Left $ InfiniteType a x
        else Right $ Substitution $ M.singleton a x
unifyMonos x (TVar a) =
    if S.member a $ vars x
        then Left $ InfiniteType a x
        else Right $ Substitution $ M.singleton a x
unifyMonos t1@(TConstructor c1 params1) t2@(TConstructor c2 params2)
    | c1 /= c2 || length params1 /= length params2 = Left $ MonoTypeMismatch t1 t2
    | otherwise =
        foldr (\(t1, t2) ctx ->
            case (ctx, unifyMonos t1 t2) of
                (Right subs1, Right subs2) -> compose subs1 subs2
                (Left e, _) -> Left e
                (_, Left e) -> Left e
        ) (Right emptySub) $ zip params1 params2
unifyMonos (TFunction f1 x1) (TFunction f2 x2) = do
        s1 <- unifyMonos f1 f2
        s2 <- unifyMonos x1 x2
        compose s1 s2
unifyMonos a b
    | a == b = Right $ Substitution M.empty
    | otherwise = Left $ MonoTypeMismatch a b

defaultContext =
    TypeContext {
        varTypes = M.fromList
            [ ("Int", PolyType {forAll=S.empty, inner=TConstructor "Int" []})
            , ("LEmpty", PolyType {forAll=S.singleton "a", inner=TConstructor "List" [TVar "a"]})
            , ("LCons", PolyType {
                forAll=S.singleton "a",
                inner =
                    TFunction
                        (TVar "a")
                        (TFunction
                            (TConstructor "List" [TVar "a"])
                            (TConstructor "List" [TVar "a"])
                        )
                })
            , ("id", PolyType { forAll = S.singleton "a", inner=TFunction (TVar "a") (TVar "a")})
            , ("add", PolyType {
                forAll = S.empty,
                inner=TFunction
                    (TConstructor "Int" [])
                    (TFunction (TConstructor "Int" []) (TConstructor "Int" []))
                } )
            , ("inc", PolyType {
                forAll = S.empty,
                inner=TFunction
                    (TConstructor "Int" [])
                    (TConstructor "Int" [])
                } )
            , ("revApp", PolyType {
                forAll = S.fromList ["a", "b"],
                inner=TFunction
                    (TVar "a")
                    (TFunction
                        (TFunction (TVar "a") (TVar "b"))
                        (TVar "b")
                    )
                } )
            ]
    }

getType :: TypeContext -> Ast -> Either TypeError (Substitution, PolyType)
getType tctx a =
    fst $ runState (runExceptT $ getTypeM tctx a) initTypeSolverState

getTypeM :: TypeContext -> Ast -> ExceptT TypeError (State TypeSolverState) (Substitution, PolyType)
getTypeM _ (RepTree _ ABottom) =
    return (emptySub, PolyType {forAll=S.empty, inner=TConstructor "Bottom" []})

getTypeM _ (RepTree _ (ANum _)) =
    return (emptySub, PolyType {forAll=S.empty, inner=TConstructor "Int" []})

getTypeM tctx (RepTree _ (AVar name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> return (emptySub, ty)
        Nothing -> throwError $ UnboundVar name

getTypeM tctx (RepTree _ (AConstructor name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> return (emptySub, ty)
        Nothing -> throwError $ UnboundVar name

getTypeM tctx (RepTree _ (ALambda [] body)) = getTypeM tctx body
getTypeM tctx (RepTree x (ALambda (var:vars) body)) = do
    newVar <- freshVar
    let tctx' = insertVar var (PolyType {forAll=S.empty, inner=TVar newVar}) tctx

    (sub, bodyT) <- getTypeM tctx' (RepTree x (ALambda vars body))

    let varT = applySub sub (TVar newVar)
        sub' = Substitution $ M.delete newVar $ subMap sub

    return (sub', combinePoly TFunction (bindFrees varT) bodyT)


getTypeM tctx (RepTree _ (ACall [f])) = getTypeM tctx f
getTypeM tctx (RepTree ctx (ACall fs)) = do
    let f = RepTree ctx (ACall (init fs))
        x = last fs

    (s1, fType) <- getTypeM tctx f
    (s2, xType) <- getTypeM tctx x

    let xType' = applySub s1 xType
    let fType' = applySub s2 fType

    let xType'' = renameForAlls (forAll fType') xType'

    s12 <- liftEither (s1 `compose` s2)

    resName <- freshVar

    sub <- liftEither $ unifyMonos (inner fType') (TFunction (inner xType'') (TVar resName))

    let res = applySub sub (TVar resName)
        fa = forAll fType `S.difference` M.keysSet (subMap sub)
        sub' = Substitution $ M.delete resName $ subMap sub

    subRes <- liftEither (s12 `compose` sub')


    return (subRes, PolyType { forAll = fa, inner = res })

getTypeM tctx (RepTree _ (ALet binds exp)) = do
    let boundTypes = M.fromList $ tBinds binds
        tctx' = M.foldrWithKey
                (\name ty tctx -> tctx { varTypes = M.insert name ty $ varTypes tctx })
                tctx boundTypes
        boundVars = vBinds binds

    boundVarUni <- sequence $ (\(name, val) -> (name, ) <$> getTypeM tctx' val) <$> boundVars

    let subs = (fst . snd) <$> boundVarUni
    composed <- liftEither $ foldr (\sub composed -> composed >>= compose sub) (Right emptySub) subs

    let boundVarTypes = (snd <$>) <$> boundVarUni

    let tctx'' = foldr
                (\(name, ty) tctx -> tctx { varTypes = M.insert name ty $ varTypes tctx })
                tctx' boundVarTypes

    (tSub, res) <- getTypeM tctx'' exp

    rSub <- liftEither $ composed `compose` composed

    return (rSub, applySub composed res)
