module Parser.BNFGenerator (generator) where
-- assumes the first production is the root

import Parser.BNF
import Parser.Core
import Parser.Primitives (string)

import Control.Applicative
import Data.Maybe
import Data.Tree

type AST = Tree String
type Parser' = Parser Char AST
type Mem = [(String,Parser')]


generator :: BNF -> Parser'
generator = parserGrammar

parserGrammar :: [Rule] -> Parser'
parserGrammar g = parserRule mem (head g)
    where mem = map (\(name,defs) -> (name, parserRule mem (name,defs))) g

parserRule :: Mem -> Rule -> Parser'
parserRule mem (name,alts)
    = foldl1 (<|>) $ map (parserDef mem name) alts

parserDef :: Mem -> String -> [Primary] -> Parser'
parserDef mem name prims
    = comb name $ map (parserPrim mem) prims

parserPrim :: Mem -> Primary -> Parser'
parserPrim mem (NT iden) = fromMaybe (error (iden ++ " is undefined")) (lookup iden mem)
parserPrim _ (T lit) = leaf <$> string lit

----------------------
-- Helper functions --
----------------------
comb :: String -> [Parser'] -> Parser'
comb iden ps
    = Node iden <$> sequence ps

leaf :: String -> AST
leaf label = Node label []
