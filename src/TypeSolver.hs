module TypeSolver where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative

import Debug.Trace

import Type
import Ast
import RepTree

type Substitution = M.Map String MonoType

compose :: Substitution -> Substitution -> Either TypeError Substitution
compose s1 s2 =
    let inter = M.intersectionWith (,) s1 s2
    in if M.size inter == 0
        then Right $ M.union s1 s2
        else
            let firstName = (M.keys inter !! 0)
                (t1, t2) = inter M.! firstName
            in do
                sub <- unifyMonos t1 t2
                let t = applySub sub t1
                    s1' = M.insert firstName t s1
                    s2' = M.delete firstName s2

                s <- compose s1' s2'
                compose s sub


class Substitutable a where
    applySub :: Substitution -> a -> a

instance Substitutable MonoType where
    applySub sub (TVar x) =
        case M.lookup x sub of
            Just a -> applySub sub a
            Nothing -> TVar x
    applySub sub (TConstructor name params) =
        TConstructor name $ map (applySub sub) params
    applySub sub (TFunction f x) =
        TFunction (applySub sub f) (applySub sub x)

instance Substitutable PolyType where
    applySub sub = mapInner (applySub sub)

data TypeContext = TypeContext { varTypes :: M.Map String PolyType}

instance Substitutable TypeContext where
    applySub sub tctx = tctx { varTypes = M.map (mapInner (applySub sub)) $ varTypes tctx }

insertVar :: String -> PolyType -> TypeContext -> TypeContext
insertVar name ty tctx =
    tctx { varTypes = M.insert name ty $ varTypes tctx }

newVarName :: TypeContext -> String
newVarName tctx =
    let innerNames = mconcat $ fmap (vars . inner . snd) $ M.toList $ varTypes tctx
        outerNames = M.keysSet $ varTypes tctx
        names = innerNames <> outerNames
    in head $ filter (flip S.notMember names) varNames


unifyMonos :: MonoType -> MonoType -> Either TypeError Substitution
unifyMonos (TVar a) x =
    if S.member a $ vars x
        then Left $ InfiniteType a x
        else Right $ M.singleton a x
unifyMonos x (TVar a) =
    if S.member a $ vars x
        then Left $ InfiniteType a x
        else Right $ M.singleton a x
unifyMonos t1@(TConstructor c1 params1) t2@(TConstructor c2 params2)
    | c1 /= c2 || length params1 /= length params2 = Left $ MonoTypeMismatch t1 t2
    | otherwise =
        foldr (\(t1, t2) ctx ->
            case (ctx, unifyMonos t1 t2) of
                (Right subs1, Right subs2) -> compose subs1 subs2
                (Left e, _) -> Left e
                (_, Left e) -> Left e
        ) (Right M.empty) $ zip params1 params2
unifyMonos (TFunction f1 x1) (TFunction f2 x2) = do
        s1 <- unifyMonos f1 f2
        s2 <- unifyMonos x1 x2
        compose s1 s2
unifyMonos a b = Left $ MonoTypeMismatch a b

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
            ]
    }


getType :: TypeContext -> Ast -> Either TypeError (Substitution, PolyType)
getType _ (RepTree _ ABottom) =
    Right (M.empty, PolyType {forAll=S.empty, inner=TConstructor "Bottom" []})

getType _ (RepTree _ (ANum _)) =
    Right (M.empty, PolyType {forAll=S.empty, inner=TConstructor "Int" []})

getType tctx (RepTree _ (AVar name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> Right (M.empty, ty)
        Nothing -> Left $ UnboundVar name

getType tctx (RepTree _ (AConstructor name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> Right (M.empty, ty)
        Nothing -> Left $ UnboundVar name

getType tctx (RepTree _ (ALambda [] body)) = getType tctx body
getType tctx (RepTree x (ALambda (var:vars) body)) = do
    let newVar = newVarName tctx
    let tctx' = insertVar var (PolyType {forAll=S.empty, inner=TVar newVar}) tctx

    (sub, bodyT) <- getType tctx' (RepTree x (ALambda vars body))

    let res =
            PolyType
                { forAll = S.insert newVar $ forAll bodyT
                , inner = TFunction (applySub sub $ TVar newVar) (inner bodyT)
                }

    return (sub, res)

getType tctx (RepTree _ (ACall [f])) = getType tctx f
getType tctx (RepTree x (ACall calls)) =
    let func = RepTree x (ACall $ init calls)
        arg = last calls
    in do

        -- Might want to care about forAlls?
        (s1, PolyType {inner=funcT, forAll=fa }) <- getType tctx func
        (s2, polyArgT) <- getType tctx arg

        let PolyType {inner=argT, forAll=fa'} = renameAllFrees fa (applySub s2 polyArgT)
        let resV = head $ filter (not . flip S.member (S.union fa fa')) varNames

        s4 <- unifyMonos funcT (TFunction argT (TVar resV))

        let resT = bindFrees $ applySub s4 (TVar resV)

        return (s4, resT)
