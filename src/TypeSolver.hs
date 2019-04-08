module TypeSolver where

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative

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
            ]
    }


getType :: TypeContext -> Ast -> Either TypeError (Substitution, PolyType)
getType _ (RepTree _ ABottom) =
    Right (emptySub, PolyType {forAll=S.empty, inner=TConstructor "Bottom" []})

getType _ (RepTree _ (ANum _)) =
    Right (emptySub, PolyType {forAll=S.empty, inner=TConstructor "Int" []})

getType tctx (RepTree _ (AVar name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> Right (emptySub, ty)
        Nothing -> Left $ UnboundVar name

getType tctx (RepTree _ (AConstructor name)) =
    case M.lookup name $ varTypes tctx of
        Just ty -> Right (emptySub, ty)
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
