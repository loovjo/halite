module HighRep where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import RepTree
import Type
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

data HighBind
    = HVBind String High
    | HTBind String PolyType

data HighCtx
    = HighCtx PolyType AstCtx
    deriving Show
