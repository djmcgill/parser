module Parser.BNF where

import Parser.Core
import Parser.Primitives

import Control.Applicative

type BNF     = [Rule]
type Rule    = (String, [Def])
type Def     = [Primary]
data Primary = NT String
             | T  String
    deriving (Eq, Show)

parseBNF :: Parser Char BNF
parseBNF  = some parseRule

parseRule :: Parser Char Rule
parseRule  = do
    whitespace
    name <- between (string "<") (string ">")
    whitespace
    string "::="
    defs <- parseDefs
    lineEnd
    return (name,defs)

parseDefs :: Parser Char [Def]
parseDefs  = do
    whitespace
    x <- parseDef
    whitespace
    string "|"
    xs <- parseDefs
    return (x:xs)
    <|> do x <- parseDef
           return [x]

parseDef  :: Parser Char Def
parseDef   = some $ do
    whitespace
    t <- parsePrimary
    return t

parsePrimary :: Parser Char Primary
parsePrimary  = (T <$> parseLiteral) <|> (NT <$> between (string "<") (string ">"))

parseLiteral :: Parser Char String
parseLiteral  = (between (string "\"") (string "\""))
                 <|>
                (between (string "\'") (string "\'"))

whitespace :: Parser Char String
whitespace  = many $ char ' '

lineEnd :: Parser Char String
lineEnd  = do
    whitespace
    string "\r\n" <|> string "\n\r" <|> string "\n"

{-
sampleGrammar:
<plus>   ::= <mult>   "+" <plus> | <mult>
<mult>   ::= <number> "*" <mult> | <number>
<number> ::= <digit>    <number> | <digit>
<digit>  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
-}

sampleGrammar :: BNF
sampleGrammar
  =  [ ("plus"  , [[NT "mult", T "+", NT "plus"], [NT "mult"]])
     , ("mult"  , [[NT "number"  , T "*", NT "mult"  ], [NT "number"]])
     , ("number", [[NT "digit", NT "number"],[NT "digit"]])
     , ("digit" , [[T "0"],[T "1"],[T "2"],[T "3"],[T "4"]
                    ,[T "5"],[T "6"],[T "7"],[T "8"],[T "9"]])]
