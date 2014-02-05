module DataIO where

import Data
import DataUtil
import Data.Maybe
import Data.Char

import Data.List
import Text.ParserCombinators.ReadP

-- READ/SHOW
readVar1 :: ReadS Name 
readVar1 i = concat [lex s1 | (",", s1) <- lex i]
	
instance Read Expr where
	readsPrec _ s = readsExpr s

instance Read Program where
	readsPrec _ s = readProgram s

readExpr :: ReadP Expr
readExpr = readS_to_P readsExpr

readsExpr :: ReadS Expr
readsExpr i = catMaybes [merge n (readArgs s)  s | (n, s) <- lex i] where
	merge n@('g':_) [(args, s1)] _ = Just (GCall n args, s1)
	merge n@('f':_) [(args, s1)] _ = Just (FCall n args, s1)
	merge n@(x:_) [(args, s1)] _ | isUpper x = Just (Ctr n args, s1)
	merge n@(x:_) [] s | isLower x = Just (Var n, s)
	merge _ _ _ = Nothing

readArgs :: ReadS [Expr]
readArgs = readP_to_S $ between (char '(') (char ')') (sepBy readExpr (char ','))

readVars :: ReadS [Name]
readVars = readP_to_S $ between (char '(') (char ')') (sepBy (readS_to_P lex) (char ','))

readFDef :: ReadS FDef
readFDef i = [ (FDef n vars body, s4) | 
	(n@('f':_), s) <- lex i, 
	(vars, s1) <- readVars s, 
	("=", s2) <- lex s1,
	(body, s3) <- readsExpr s2,
	(";", s4) <- lex s3]
	
readSPat :: ReadS Pat
readSPat i = [(Pat n vars, s2)|
	(n, s) <- lex i,
	(vars, s2) <- readVars s]
-- read g-function
readGDef i = [ (GDef n p vs body, s6) |
	(n@('g':_), s) <- lex i,
	("(", s1) <- lex s,
	(p, s2) <- readSPat s1,
	(vs, s3) <- readP_to_S (manyTill (readS_to_P readVar1)  (char ')')) s2,
	("=", s4) <- lex s3,
	(body, s5) <- readsExpr s4,
	(";", s6) <- lex s5
	]

readProgram s = [readP1 (Program [] []) s]

readP1 p@(Program fs gs) s = next (readFDef s) (readGDef s) where
	next [(f, s1)] _ = readP1 (Program (fs++[f]) gs) s1
	next _ [(g, s1)] = readP1 (Program fs (gs++[g])) s1
	next _ _ = (p, s)
	
printTree t = unlines $ take 1000 $ pprintTree "" "" t

pprintTree :: String -> String -> Graph Conf -> [String]
pprintTree indent msg (Node expr next) = make next where
	make (Fold _ ren) = (indent ++ msg) : [indent ++ "|__" ++  (show expr) ++ "__↑" ++ (show ren)]
	make Stop = (indent ++ msg) : [indent ++ "|__" ++  (show expr)]
	make (Transient t) = (indent ++ msg) : (indent ++ "|__" ++ show expr) : (pprintTree (indent ++ " ") "|" t)
	make (Decompose ts) = (indent ++ msg) :  (indent ++ "|__" ++ show expr): (concat (map (pprintTree (indent ++ " ") "|") ts))
	make (Variants cs) = 
		(indent ++ msg) :  (indent ++ "|__" ++  show expr) : (concat (map (\(x, t) -> pprintTree (indent ++ " ") ("?" ++ show x) t) cs))

instance Show FDef where
	show (FDef n args body) = (fn n) ++ "(" ++ intercalate ", " args ++ ") = " ++ (show body) ++ ";"

instance Show GDef where
	show (GDef n p args body) = (fn n) ++ "(" ++ intercalate ", " (show p:args) ++ ") = " ++ (show body) ++ ";"

instance Show Program where
	show (Program fs gs) = intercalate "\n" $ (map show fs) ++ (map show gs)
	
-- Latex
pprintLTree :: Graph Conf -> String
pprintLTree (Node expr next) = make next where
	make (Fold _ _) = "node[conf]{" ++ (show expr) ++ "}"
	make Stop = "node[conf]{" ++ (show expr) ++ "}"
	make (Transient t) = "node[conf]{" ++ (show expr) ++ "}\nchild[->]{" ++ (pprintLTree t) ++ "}"
	make (Decompose ts) = "node[conf]{" ++ (show expr) ++ "}" ++ 
		(concat (map (\t -> "\nchild[->]{" ++ (pprintLTree t) ++ "}") ts))
	make (Variants [(x1, t1), (x2, t2)]) = 
		"node[conf]{" ++ (show expr) ++ "}" ++ 
			("\nchild[->]{" ++ (pprintLTree t1) ++ "\nedge from parent node[left,label,xshift=-5mm]{" ++ (show x1) ++ "}}") ++
			("\nchild[->]{" ++ (pprintLTree t2) ++ "\nedge from parent node[right,label,xshift=5mm]{" ++ (show x2) ++ "}}")
