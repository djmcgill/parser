module Parser.EBNF where
-- EBNF, from ISO/IEC 14977:1996(E)
-- doesn't quite conform: see 6.2

import Parser.BNF
import Parser.Core
import Parser.Primitives

import Control.Applicative
import Data.Char(isAlpha, isAlphaNum)

type EBNF = [ERule]
type ERule
-- rule = meta-iden "=" def-list ";"
    = (EIden, EDefList)

type EDefList
-- def-list = def "|" def "|" ... "|" def
    = [EDef]

type EDef
--- def = term "," term "," ... "," term
    = [ETerm]

data ETerm
-- term = factor
--      | factor "-" except
    = ETFactor EFactor
    | ETExcept EFactor EExcept
    deriving (Eq,Show)

type EExcept
-- except = factor (for now)
    = EFactor

data EFactor
-- factor = int "*" primary
--        | primary
    = EFRept Int EPrimary
    | EFPrim EPrimary
    deriving (Eq,Show)

data EPrimary
-- primary = optional-seq
--         | repeated-seq
--         | grouped-seq
--         | meta-iden
--         | terminal-str
--         | special-seq
--         | empty-seq
    = EPOpt     EDefList
    -- optional-seq = "[" def-list "]"
    | EPRep     EDefList
    -- repeated-seq = "{" def-list "}"
    | EPGroup   EDefList
    -- grouped-seq = "(" def-list ")"
    | EPIden    EIden
    | EPTermStr ETermStr
    | EPSpec    ESpecial
    | EPEmpty
    deriving (Eq,Show)

type EIden
    -- an ordered list of one or more meta-identifier
    -- characters subject to the condition that the first
    -- one is a letter
    = String

data ETermStr
    -- type ETermStr = String?
    -- term = "'"  (char -  "'")+ "'"
    --      | "\"" (char - "\"")+ "\""
    = ETSingle [Char]
    | ETDouble [Char]
    deriving (Eq,Show)

type ESpecial
    -- special-seq = "?" (char - "?")* "?"
    = [Char]

-----------------------
-- parsing functions --
-----------------------

parseEBNF :: Parser Char EBNF
parseEBNF = some parseERule

parseERule :: Parser Char ERule
parseERule = do
    name <- parseEIden
    lit "="
    defs <- parseEDefList
    lit ";"
    return (name,defs)

parseEDefList :: Parser Char [EDef]
parseEDefList = sepBy (lit "|") parseEDef

parseEDef :: Parser Char EDef
parseEDef = sepBy (lit ",") parseETerm

parseETerm :: Parser Char ETerm
parseETerm =
    (do f <- parseEFact
        lit "-"
        e <- parseEExcept
        return (ETExcept f e))
     <|>
    (ETFactor <$> parseEFact)

parseEFact :: Parser Char EFactor
parseEFact =
    (do i <- nat
        lit "*"
        prim <- parseEPrim
        return (EFRept i prim))
     <|>
    (EFPrim <$> parseEPrim)

parseEExcept :: Parser Char EFactor
parseEExcept = parseEFact

parseEPrim :: Parser Char EPrimary
parseEPrim = foldl1 (<|>)
    [ parseEPOpt
    , parseEPRep
    , parseEPGroup
    , EPIden <$> parseEIden
    , EPTermStr <$> parseETermStr
    , parseEPSpec
    , eWhitespace >> return EPEmpty ]

parseEPOpt = do
    lit "["
    defs <- parseEDefList
    lit "]"
    return (EPOpt defs)

parseEPRep = do
    lit "{"
    defs <- parseEDefList
    lit "}"
    return (EPRep defs)

parseEPGroup = do
    lit "("
    defs <- parseEDefList
    lit ")"
    return (EPGroup defs)

parseEPSpec = EPSpec <$> between (char '?') (char '?')

parseEIden :: Parser Char EIden
parseEIden = do
    l <- sat isAlpha
    ls <- many (sat isAlphaNum)
    return (l:ls)

parseETermStr =
    (ETSingle <$> between (char '\'') (char '\''))
     <|>
    (ETDouble <$> between (char '\"') (char '\"'))


----------------------
-- helper functions --
----------------------
lit :: String -> Parser Char String
lit = token . string

token :: Parser Char String -> Parser Char String
token p = do
    eWhitespace
    p' <- p
    eWhitespace
    return p'

eWhitespace :: Parser Char [String]
eWhitespace = many ((sat (`elem` " \t\r\n") >> return "")
                     <|>
                    (comment))
comment :: Parser Char String
comment = between (string "(*") (string  "*)")



