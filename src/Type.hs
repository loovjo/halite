module Type where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List

-- A type with (maybe) free variables
data MonoType
    = TVar String
    | TConstructor String [MonoType] -- ^ Constructor with generics
    | TFunction MonoType MonoType
    deriving Eq

data PolyType =
    PolyType
        { forAll :: S.Set String
        , inner :: MonoType
        }
    deriving Eq

mapInner :: (MonoType -> MonoType) -> PolyType -> PolyType
mapInner f p = p { inner = f $ inner p }

instance Show PolyType where
    show (PolyType { forAll=fa, inner=mon}) =
        if S.size fa == 0
            then show mon
            else "(∀ " ++ (intercalate " " $ S.toList fa) ++ ". " ++ show mon ++ ")"

instance Show MonoType where
    show = showMonoParens

showMonoParens (TVar name) = name
showMonoParens (TConstructor cons []) = cons
showMonoParens ty = "(" ++ showMono ty ++ ")"

showMono (TVar name) = name
showMono (TConstructor cons []) = cons
showMono (TConstructor cons params) = cons ++ " " ++ intercalate " " (map showMonoParens params)
showMono (TFunction f x) = showMonoParens f ++ " -> " ++ showMono x

vars :: MonoType -> S.Set String
vars (TVar v) = S.singleton v
vars (TConstructor _ params) = mconcat $ map vars params
vars (TFunction f x) = vars f <> vars x

freeVars :: PolyType -> S.Set String
freeVars (PolyType { forAll=fa, inner=mon})
    = vars mon `S.difference` fa

bindFrees :: MonoType -> PolyType
bindFrees m =
    let freeVars = vars m
    in PolyType { inner = m, forAll = freeVars }

renameFree :: String -> String -> MonoType -> MonoType
renameFree from to x =
    go x
    where
        go (TVar a)
            | a == from = TVar to
            | otherwise = TVar a
        go (TConstructor c params) = TConstructor c $ map go params
        go (TFunction x a) = TFunction (go x) (go a)

letters = "abcdefghijklmnopqrstuvqxyz"
varNames :: [String]
varNames = [1..] >>= flip replicateM letters

renameForAlls :: S.Set String -> PolyType -> PolyType
renameForAlls taken p =
    PolyType { inner = ty, forAll = fa }
    where
        update var (ty, fa) =
            let taken' = taken `S.union` (S.delete var fa)
            in if var `S.member` taken'
                then
                    let var' = head $ filter (not . flip S.member taken') varNames
                        ty' = renameFree var var' ty
                    in (ty', S.insert var' (S.delete var fa))
                else (ty, fa)
        (ty, fa) = foldr update (inner p, forAll p) (forAll p)



data TypeError
    = PolyTypeMismatch PolyType PolyType
    | MonoTypeMismatch MonoType MonoType
    | GensMismatch [(String, (MonoType, MonoType))]
    | InfiniteType String MonoType
    | UnboundVar String
    deriving (Show, Eq)

