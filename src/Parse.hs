{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Parser
import Ast
import Type
import RepTree

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Traversable
import Control.Applicative

reserved = ["let", "in", "match"]

data HParseError
    = UnexpectedEnd
    | ExpectedAnyOf [T.Text]
    | ExpectedLetter
    | ExpectCagetory GeneralCategory
    | ReservedWord String
    deriving (Show, Eq)

instance ParseError HParseError where
    unexpectedEnd = UnexpectedEnd
    expectedWord x = ExpectedAnyOf [x]
    expectedLetter = ExpectedLetter
    expectedCategory = ExpectCagetory

    groupe (e1, idx1) (e2, idx2) =
        if idx1 == idx2
            then case (e1, e2) of
                (ExpectedAnyOf as, ExpectedAnyOf bs) -> Just (ExpectedAnyOf $ nub $ as ++ bs, idx1)
                (a, b) | a == b -> Just (a, idx1)
                _ -> Nothing
            else Nothing

intoParseAst :: Parser HParseError AstBranch -> Parser HParseError Ast
intoParseAst parser =
    Parser {
        doParse = \t i ->
            case doParse parser t i of
                Left x -> Left x
                Right (a, i') ->
                    Right $ (RepTree (AstCtx { cspan = (i, i')}) a, i')
    }


-- Parses as litte code as possible
-- eg. a b -> AVar a, rest = "b"
parseLazy :: Parser HParseError Ast
parseLazy =
    token $ foldl1' (<|>)
        [ parseConstructor
        , parseVar
        , parseNum
        , parseLet
        , parens parseAst
        ]

-- Parses as much code as possible
-- eg. a b -> ACall (AVar a) (AVar b), rest = ""
parseAst :: Parser HParseError Ast
parseAst =
    token $ foldl1' (<|>)
        [ parseCall
        , parseLet
        , parseLambda
        , parseVar
        , parseNum
        , parseConstructor
        , parens parseAst
        ]

parseVar :: Parser HParseError Ast
parseVar =
    intoParseAst (
        AVar <$> matchVName
    )

parseNum :: Parser HParseError Ast
parseNum = intoParseAst (ANum <$> matchInt)

parseConstructor :: Parser HParseError Ast
parseConstructor = intoParseAst (AConstructor <$> matchCName)

parseLet :: Parser HParseError Ast
parseLet = intoParseAst parseLetBranch
    where parseLetBranch = do
            token $ matchText "let"
            binds <- separated parseBind $ token $ matchChar ';'

            matchChar ';' <|> pure ' '

            token $ matchText "in"

            body <- parseAst

            return $ ALet binds body

parseLambda :: Parser HParseError Ast
parseLambda = intoParseAst parseLambdaBranch
    where parseLambdaBranch = do
            token $ matchChar '\\'
            vars <- some $ token $ some matchLetter
            token $ matchChar '.'
            body <- parseAst

            return $ ALambda vars body

parseCall :: Parser HParseError Ast
parseCall =
    intoParseAst (
        ACall <$>
            guardE (\fs ->
                if length fs < 2
                    then Just UnexpectedEnd
                    else Nothing
            ) (some parseLazy)
    )

parseBind :: Parser HParseError ABind
parseBind = parseVBind <|> parseTBind

parseVBind :: Parser HParseError ABind
parseVBind = do
    v <- matchVName
    token $ matchChar '='
    e <- parseAst
    return $ AVBind v e

parseTBind :: Parser HParseError ABind
parseTBind = do
    v <- matchVName
    token $ matchText $ T.pack "::"
    t <- parsePolyType
    return $ ATBind v t

parsePolyType :: Parser HParseError PolyType
parsePolyType = do
    mty <- parseMonoType
    return $ bindFrees mty

parseMonoType :: Parser HParseError MonoType
parseMonoType =
    foldl1' (<|>)
        [ parseMonoFunc
        , parseMonoVar
        , parseMonoCons
        , parens parseMonoType ]

parseMonoTypeLazy :: Parser HParseError MonoType
parseMonoTypeLazy =
    foldl1' (<|>)
        [ parseMonoVar
        , parseMonoCons
        , parens parseMonoType ]

parseMonoVar :: Parser HParseError MonoType
parseMonoVar = TVar <$> token matchVName

parseMonoCons :: Parser HParseError MonoType
parseMonoCons = do
    h <- token matchCName
    rest <- many parseMonoTypeLazy
    return $ TConstructor h rest

parseMonoFunc :: Parser HParseError MonoType
parseMonoFunc = do
    f <- parseMonoTypeLazy
    matchText $ T.pack "->"
    x <- parseMonoType
    return $ TFunction f x


matchLetter :: ParseError e => Parser e Char
matchLetter =
    emap' (const expectedLetter) $
        foldl1' (<|>) $ map matchGCategory cats
    where cats = [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter]

matchVName :: Parser HParseError String
matchVName = guardE
        (\name -> if name `elem` reserved then Just $ ReservedWord name else Nothing)
        matchVName'
    where matchVName' = do
            fst <- (matchGCategory LowercaseLetter) <|> matchChar '_'
            rest <- many $ (matchLetter <|> matchChar '_' <|> matchDigit)
            return $ fst : rest


matchCName :: Parser HParseError String
matchCName = guardE
        (\name -> if name `elem` reserved then Just $ ReservedWord name else Nothing)
        matchCName'
    where matchCName' = do
            fst <- (matchGCategory UppercaseLetter)
            rest <- many $ (matchLetter <|> matchChar '_' <|> matchDigit)
            return $ fst : rest

