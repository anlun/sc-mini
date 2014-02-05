module Data where

import Data.Char
import Data.List

type Name = String

data Expr = Var Name
          | Ctr Name [Expr]
          | FCall Name [Expr]
          | GCall Name [Expr]
          | Let       (Name, Expr)  Expr
          | MultiLet [(Name, Expr)] Expr
          deriving (Eq)

fn :: String -> String	
fn (_:s:ss) = (toLower s) : ss

instance Show Expr where
	show (Ctr "Nil" []) = "``\'\'"
	show (Ctr "Cons" [Ctr "B" [], Ctr "Nil" []]) = "``B\'\'"
	show (Ctr "Cons" [Ctr "A" [], (Ctr "Cons" [Ctr "B" [], Ctr "Nil" []])]) = "``AB\'\'"
	show (Ctr "Cons" [Ctr "A" [], (Ctr "Cons" [Ctr "A" [], (Ctr "Cons" [Ctr "B" [], Ctr "Nil" []])])]) = "``AAB\'\'"
	show (Ctr "Cons" [x, y]) = (show x) ++ ":" ++ (show y)
	show (Ctr "A" []) = "\'A\'"
	show (Ctr "B" []) = "\'B\'"
	show (Var n) = n
	show (Ctr n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (FCall n es) = (fn n) ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (GCall n es) = (fn n) ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (Let (v, e1) e2) = "let " ++ v ++ " = " ++ (show e1) ++ " in " ++ (show e2)
	show (MultiLet  vl e) = "Mlet " ++ (foldl (\s (v, e) -> s ++ "; " ++ v ++ " = " ++ (show e)) "" vl) ++ " in " ++ (show e)


instance Show Pat where
	show (Pat "Nil" vs) = "``\'\'"
	show (Pat "Cons" [v1, v2]) = v1 ++ ":" ++ v2
	show (Pat cn vs) = cn ++ "(" ++ intercalate "," vs ++ ")"

instance Show Contract where
	show (Contract n p) = n ++ " == " ++ (show p)

instance Show a => Show (Step a) where
	show (Transient a) = "=> " ++ (show a)
	show (Variants vs) = intercalate "\n" $ map (\(c, e) -> (show c) ++ " => " ++ (show e)) vs 
	show Stop = "!"
	show (Decompose ds) = show ds
	show (Fold e _) = "↑" ++ (show e)

data Pat = Pat Name [Name] deriving (Eq)
data GDef = GDef Name Pat [Name] Expr deriving (Eq)
data FDef = FDef Name [Name] Expr deriving (Eq)
data Program = Program [FDef] [GDef] deriving (Eq)

type Renaming = [(Name, Name)]
type Subst = [(Name, Expr)]
type NameSupply = [Name]

type Conf = Expr
type Value = Expr
type Task = (Conf, Program)
type Env = [(Name, Value)]

data Contract = Contract Name Pat
data Step a = Transient a | Variants [(Contract, a)] | Stop | Decompose [a] | Fold a Renaming
data Show a => Graph a = Node a (Step (Graph a)) deriving Show
type Tree a = Graph a
type Node a = Tree a

type Machine a = NameSupply -> a -> Step a
