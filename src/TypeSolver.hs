{-# LANGUAGE TupleSections #-}

module TypeSolver where

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State.Lazy
import Control.Monad.Except

import Type
import Dst
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

-- Start with cvid = 2 to avoid collisions with defaultContext
initTypeSolverState = TypeSolverState { currentVarID = 3 }

freshVar :: ExceptT e (State TypeSolverState) String
freshVar = do
    current <- currentVarID <$> get
    modify (\tss -> tss { currentVarID = succ current })
    return (varNames !! current)

updateState :: S.Set String -> ExceptT e (State TypeSolverState) ()
updateState names = do
    current <- (varNames !!) <$> currentVarID <$> get
    if S.member current names
        then freshVar >> updateState (S.delete current names)
        else return ()

usedVars :: ExceptT e (State TypeSolverState) (S.Set String)
usedVars = do
    current <- currentVarID <$> get
    return $ S.fromList $ (varNames !!) <$> [0..current-1]



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
unifyMonos (TVar a) (TVar b) =
    if a == b
        then Right emptySub
        else Right $ Substitution $ M.singleton b (TVar a)
unifyMonos x (TVar a) =
    if S.member a $ vars x
        then Left $ InfiniteType a x
        else Right $ Substitution $ M.singleton a x
unifyMonos (TVar a) x =
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
    let i2i = PolyType {
                forAll = S.empty,
                inner=TFunction
                    (TConstructor "Int" [])
                    (TFunction (TConstructor "Int" []) (TConstructor "Int" []))
                }
    in TypeContext {
        varTypes = M.fromList
            [ ("empty", PolyType {forAll=S.singleton "a", inner=TConstructor "List" [TVar "a"]})
            , ("cons", PolyType {
                forAll=S.singleton "a",
                inner =
                    TFunction
                        (TVar "a")
                        (TFunction
                            (TConstructor "List" [TVar "a"])
                            (TConstructor "List" [TVar "a"])
                        )
                })
            , ("map", PolyType {
                forAll=S.fromList ["a", "b"],
                inner =
                    TFunction
                        (TFunction (TVar "a") (TVar "b"))
                        $ TFunction
                            (TConstructor "List" [TVar "a"])
                            (TConstructor "List" [TVar "b"])
                })
            , ("id", PolyType { forAll = S.singleton "a", inner=TFunction (TVar "a") (TVar "a")})
            , ("inc", PolyType {
                forAll=S.empty,
                inner =
                    TFunction
                        (TConstructor "Int" [])
                        (TConstructor "Int" [])
                })
            , ("+", i2i)
            , ("-", i2i)
            , ("*", i2i)
            , ("/", i2i)
            , ("%", i2i)
            , ("^", i2i)
            , ("$", PolyType {
                forAll = S.fromList ["a", "b"],
                inner=TFunction
                    (TFunction (TVar "a") (TVar "b"))
                    (TFunction
                        (TVar "a")
                        (TVar "b")
                    )
                } )
            , (".", PolyType {
                forAll = S.fromList ["a", "b", "c"],
                inner=TFunction
                    (TFunction (TVar "b") (TVar "c"))
                    (TFunction
                        (TFunction (TVar "a") (TVar "b"))
                        (TFunction (TVar "a") (TVar "c"))
                    )
                } )
            ]
    }

getType :: TypeContext -> Dst -> Either TypeError (Substitution, PolyType)
getType tctx a =
    fst $ runState (runExceptT $ getTypeM tctx a) initTypeSolverState

getTypeM _ (RepTree _ (DNum _)) =
    return (emptySub, PolyType {forAll=S.empty, inner=TConstructor "Int" []})

getTypeM tctx (RepTree _ (DVar name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> return (emptySub, ty)
        Nothing -> throwError $ UnboundVar name

getTypeM tctx (RepTree _ (DConstructor name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> return (emptySub, ty)
        Nothing -> throwError $ UnboundVar name

getTypeM tctx (RepTree _ (DLambda var body)) = do
    newVar <- freshVar

    let tctx' = insertVar var (PolyType {forAll=S.empty, inner=TVar newVar}) tctx

    (sub, bodyT) <- getTypeM tctx' body

    let varT = applySub sub (TVar newVar)
        sub' = Substitution $ M.delete newVar $ subMap sub
        fa = vars varT `S.union` forAll bodyT

    return (sub', PolyType {forAll=fa, inner=TFunction varT $ inner bodyT})


getTypeM tctx (RepTree _ (DCall f x)) = do
    (s1, fType) <- getTypeM tctx f
    (s2, xType) <- getTypeM tctx x

    let xType' = applySub s1 xType
    let fType' = applySub s2 fType

    let s1' = Substitution $ M.withoutKeys (subMap s1) $ vars $ inner xType'
        s2' = Substitution $ M.withoutKeys (subMap s2) $ vars $ inner fType'

    used <- usedVars

    let xType'' = renameForAlls used xType'

    updateState $ vars $ inner xType''

    s12 <- liftEither (s1' `compose` s2')

    resName <- freshVar

    sub <- liftEither $ unifyMonos (inner fType') (TFunction (inner xType'') (TVar resName))

    let res = applySub sub (TVar resName)
        fa = (forAll fType' `S.union` forAll xType'') `S.difference` M.keysSet (subMap sub)
        free = S.fromList $ concatMap (S.toList . freeVars) $ M.elems $ varTypes tctx
        sub' = Substitution $ M.restrictKeys (subMap sub) free

    subRes <- liftEither (s12 `compose` sub')

    let resT = PolyType { forAll = fa, inner = res }

    return (subRes, resT)


getTypeM tctx (RepTree _ (DLet tbinds vbinds exp)) = do
    let tctx' = M.foldrWithKey
                (\name ty tctx -> tctx { varTypes = M.insert name ty $ varTypes tctx })
                tctx $ M.fromList tbinds

    boundVarUni <- sequence $ (\(name, val) -> (name, ) <$> getTypeM tctx' val) <$> vbinds

    let subs = (fst . snd) <$> boundVarUni
    composed <- liftEither $ foldr (\sub composed -> composed >>= compose sub) (Right emptySub) subs

    let boundVarTypes = (snd <$>) <$> boundVarUni

    tctx'' <- liftEither $ foldr
                (\(name, ty) ct ->
                    ct >>= (\tctx ->
                        case M.lookup name $ varTypes tctx of
                            Nothing -> Right $ insertVar name ty tctx
                            Just ty' -> do
                                let ty'' = renameForAlls (forAll ty) ty'
                                sub <- unifyMonos (inner ty) (inner ty'')
                                if applySub sub (inner ty) == inner ty''
                                    then Right tctx
                                    else Left $ PolyTypeMismatch ty ty'
                    )
                )
                (Right tctx') boundVarTypes

    (tSub, res) <- getTypeM tctx'' exp

    rSub <- liftEither $ composed `compose` tSub

    return (rSub, applySub composed res)
