module HType where

import Data.List

data HType
    = TNamed String
    | TConstructor String [HType] -- ^ Constructor with generics
    | TFunction HType HType
    deriving Eq

instance Show HType where
    show = ppTypeParens

ppType :: HType -> String
ppType ty =
    case ty of
        TNamed n -> n
        TConstructor name [] -> name
        TConstructor name args -> name ++ " " ++ intercalate " " (map ppTypeParens args)
        TFunction f x -> ppTypeParens f ++ " -> " ++ ppType x


ppTypeParens :: HType -> String
ppTypeParens ty =
    case ty of
        TNamed n -> n
        TConstructor name [] -> name
        TConstructor name args -> "(" ++ name ++ " " ++ intercalate " " (map ppType args) ++ ")"
        TFunction f x -> "(" ++ ppTypeParens f ++ " -> " ++ ppType x ++ ")"
