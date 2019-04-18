module HighRep where

import RepTree
import Type
import Ast


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
