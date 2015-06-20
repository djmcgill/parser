{-# LANGUAGE LambdaCase #-}

module Parser.Primitives where

import Parser.Core

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)

----------------------
-- Specific parsers
----------------------
-- Accepts a single character
item :: Parser a a
item = Parser $ \case
    []     -> Nothing
    (x:xs) -> Just (x,xs)

-- Accept a single character that satisfies the predicate.
sat :: (a -> Bool) -> Parser a a
sat p = mfilter p item

-- Parses until the given parser matches, then returns the pair
-- (everything before matching, matching result)
endBy :: (Parser a b) -> Parser a ([a],b)
endBy p =
    (do x <- p
        return ([],x))
     <|>
    (do x <- item
        (before,y) <- endBy p
        return (x:before,y))

-- Parses anything that is seperated by 'sep' until 'end' matches.
-- e.g. parse (sepByUntil (char ' ') (char ';')) "1 2 3 4.1;" === ["1", "2", "3", "4.1"]
sepByUntil :: Parser a b -> Parser a b -> Parser a [[a]]
sepByUntil sep end =
    (do (x,_) <- endBy (sep <|> end)
        xs    <- sepByUntil sep end
        return (x:xs))
     <|>
    return []

-- Alternates between 'p' and 'sep'.
-- e.g. parse (sepBy (char ' ') digit) "1 2 3 4" === [1,2,3,4]
sepBy :: Parser a b -> Parser a c -> Parser a [c]
sepBy sep p = (:) <$> p <*> many (sep >> p)

-- Parses the given character
char :: Char -> Parser Char Char
char x = sat (==x)

string :: String -> Parser Char String
string []     = return []
string (x:xs) = (:) <$> char x <*> string xs

between :: Parser a b -> Parser a c -> Parser a [a]
between l r = l >> fst <$> endBy r

digit :: Parser Char Char
digit = sat isDigit

nat :: Parser Char Int
nat = read <$> some digit