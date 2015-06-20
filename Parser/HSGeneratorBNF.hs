module Parser.HSGeneratorBNF (printParser, parserFile) where
-- assumes the first production is the root
-- doesn't deal with literals that aren't allowed in Haskell function names

import Parser.BNF
import Data.List (intersperse, nub)
import Data.Char (ord, toUpper)

printParser :: BNF -> String
printParser = parserStr

parserFile :: BNF -> IO String
parserFile g = do writeFile fName parser
                  return fName
    where
    (c:cs) = gramIden g ""
    fName  = toUpper c : cs ++ ".hs"
    parser = parserStr g

parserStr g = header g
            . showParsers g . nl 2
            . helperFuncs $ ""

showParsers :: BNF -> ShowS
showParsers g = parserGrammar g . nl 1
              . showRParsers  g . nl 1
              . showDParsers  g . nl 1
              . showPParsers  g

showRParsers :: BNF -> ShowS
showRParsers = concatS . map parserRule

showDParsers :: BNF -> ShowS
showDParsers = concatS . map showDefs

showDefs :: Rule -> ShowS
showDefs (name,defs) =
    concatS $ zipWith (parserDef name) defs [1..]

showPParsers :: BNF -> ShowS
showPParsers = concatS . map parserPrim . filter isTerm . nub . getPrims

------------------------
-- Printing functions --
------------------------

header g =
    str "module " . gramIden g
  . str " (parser) where\n\
    \import Parser.BNF\n\
    \import Parser.Core\n\
    \import Parser.Primitives\n\
    \import Data.Tree\n\n\
    \type AST = Tree String\n\
    \type Parser' = Parser Char AST\n\n"

parserGrammar (r:_) = str "parser = " . ruleIden r . nl 1

parserRule rule
    = ruleIden rule . str " = foldl1 (<|>) " . showList' (defsIden rule) . nl 1

parserDef name prims n
    = str "parseD_" . str (escape name) . shows n
    . str " = comb"
    . str " \"" . str (escape name) . str "\" "
    . showList' (map primIden prims) . nl 1

parserPrim p@(T lit)
    = primIden p
    . str " = leaf <$> string "
    . str (show lit) -- should be the only unescaped string
    . nl 1

helperFuncs = combStr . nl 1 . leafStr

combStr = str $
    "comb :: String -> [Parser'] -> Parser'\n\
    \comb iden' children' = sequence children' >>= return . Node iden'\n"

leafStr = str $
    "leaf :: String -> AST\n\
    \leaf label = Node label []\n"

----------------------
-- Helper functions --
----------------------

getPrims :: BNF -> [Primary]
getPrims g = [prim | (_,defs) <- g
                   , prims    <- defs
                   , prim     <- prims]

concatS :: [ShowS] -> ShowS
concatS = foldl (.) (str "")

unlinesS :: [ShowS] -> ShowS
unlinesS = concatS . intersperse (nl 1)

str = showString

nl :: Int -> ShowS
nl n = concatS . replicate n $ str "\n"

primIden :: Primary -> ShowS
primIden (NT iden) = ruleIden' iden
primIden (T  lit ) = str "parseP_"  . str (escape lit)

gramIden :: BNF -> ShowS
gramIden ((name,_):_) = str "ParseG_" . str (escape name)

ruleIden :: Rule -> ShowS
ruleIden (name,_) = ruleIden' name

ruleIden' :: String -> ShowS
ruleIden' s = str "parseR_" . str (escape s)

defsIden :: Rule -> [ShowS]
defsIden (name,defs) =
    map defIden [1..length defs]
    where defIden n = str "parseD_" . str (escape name) . shows n

showList' :: [ShowS] -> ShowS
showList' ss = str "["
             . concatS (intersperse (str ",") ss)
             . str "]"

escape :: String -> String
escape = concat . map replace

replace :: Char -> String
replace c | valid c   = [c]
          | otherwise = "'" ++ show (ord c) ++ "'"

valid :: Char -> Bool
valid c = elem c $ '_': ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

isTerm :: Primary -> Bool
isTerm (T  _) = True
isTerm (NT _) = False
