module Parser.EBNFtoBNF where

import Parser.BNF
import Parser.EBNF
import Control.Monad.State

-----------------
-- EBNF to BNF --
-----------------
convEBNF :: EBNF -> BNF
convEBNF ebnf = evalState (convEBNF' ebnf) 0

convEBNF' :: EBNF -> State Int BNF
convEBNF' ebnf = do
    rules <- mapM convRule ebnf
    return (concat rules)

convRule :: ERule -> State Int [Rule]
convRule (iden,defs) = do
    rules <- convDefs defs
    let iden' = getIden rules
    return $ (iden,[[NT iden']]):rules

convDefs :: EDefList -> State Int [Rule]
-- defs = def | def | def ...
convDefs defs = do
    rules <- mapM convDef defs
    let idens = map getIden rules
        rules' = concat rules
    name <- newNm
    return $ ("defs_" ++ name,(map (\iden -> [NT iden]) idens)) : rules'

convDef :: EDef -> State Int [Rule]
-- def = term , term , term ...
convDef terms = do
    rules <- mapM convTerm terms
    let idens = map getIden rules
        rules' = concat rules
    name <- newNm
    return $ ("def_" ++ name, [map NT idens]):rules'

convTerm :: ETerm -> State Int [Rule]
convTerm (ETFactor fac) = convFac fac
convTerm (ETExcept _ _) = error "Except terms not implemented."

convFac :: EFactor -> State Int [Rule]
convFac (EFRept n prim) = do
    rules <- convPrim prim
    let iden = getIden rules
    name <- newNm
    return $ ("fac_" ++ name,[replicate n (NT iden)]) : rules
convFac (EFPrim prim) = do
    rules <- convPrim prim
    let iden = getIden rules
    name <- newNm
    return $ ("fac_" ++ name,[[NT iden]]) : rules

convPrim :: EPrimary -> State Int [Rule]
convPrim (EPOpt defs) = do
    rules <- convDefs defs
    let iden = getIden rules
    name <- newNm
    return $ ("prim_" ++ name,[[NT iden],[T ""]]) :rules
convPrim (EPRep defs) = do
    rules <- convDefs defs
    let iden = getIden rules
    name' <- newNm
    let name = "prim_" ++ name'
    return $ (name,[[NT iden, NT name],[T ""]]):rules
convPrim (EPGroup defs) = do
    rules <- convDefs defs
    let iden = getIden rules
    name <- newNm
    return $ ("prim_" ++ name,[[NT iden]]):rules
convPrim (EPIden iden) = do
    name <- newNm
    return $ [("prim_" ++ name,[[NT iden]])]
convPrim (EPTermStr termstr) = do
    name <- newNm
    return $ [("prim_" ++ name,[[T (getStr termstr)]])]
convPrim (EPSpec spec) = return []
convPrim (EPEmpty) = do
    name <- newNm
    return $ [("prim_" ++ name,[[T ""]])]

getStr (ETSingle str) = str
getStr (ETDouble str) = str

getIden :: [Rule] -> String
getIden ((iden,_):_) = iden

newNm :: State Int String
newNm = do
    n <- get
    put (n+1)
    return (show n)