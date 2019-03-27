module HType where

import qualified Data.Set as S
import Data.List

data HType
    = TNamed String
    | TUnknown
    | TConstructor String [HType] -- ^ Constructor with generics
    | TFunction HType HType
    deriving Eq

instance Show HType where
    show = ppTypeParens

ppType :: HType -> String
ppType ty =
    case ty of
        TNamed n -> n
        TUnknown -> "?"
        TConstructor name [] -> name
        TConstructor name args -> name ++ " " ++ intercalate " " (map ppTypeParens args)
        TFunction f x -> ppTypeParens f ++ " -> " ++ ppType x


ppTypeParens :: HType -> String
ppTypeParens ty =
    case ty of
        TNamed n -> n
        TUnknown -> "?"
        TConstructor name [] -> name
        TConstructor name args -> "(" ++ name ++ " " ++ intercalate " " (map ppType args) ++ ")"
        TFunction f x -> "(" ++ ppTypeParens f ++ " -> " ++ ppType x ++ ")"

frees :: HType -> S.Set String
frees ty =
    case ty of
        TNamed x -> S.singleton x
        TConstructor _ xs -> foldl S.union S.empty $ map frees xs
        TFunction f x -> frees f `S.union` frees x
        _ -> S.empty
