module Samples where

import Data
import Driving
--import Interpreter
import TreeInterpreter
import Supercompiler
import Folding
import Data.List
import Data.Maybe
import Generator
import ATransformer
import Deforester

load :: (String, String) -> State
load (p1, p2) = (read p1, read p2)

doTask f (g, p) = 
	"before:\n" ++ (show goal1) ++ "\n" ++ (show program1) ++ "\n\nafter:\n" ++ (show goal2) ++ "\n" ++ (show program2) ++ "\n\n"
	where
		(goal1, program1) = (read g, read p)
		(goal2, program2) = f (goal1, program1)		
--bt (g, p) =
--	unlines $ take 100 $ pprintTree "" "" $ foldTree $ buildFTree p nameSupply g

t1 = ("gApp(gApp(gApp(x, y), z), v)",
	" gApp(Nil(), ys) = ys;\
	\ gApp(Cons(x, xs), ys) = Cons(x, gApp(xs, ys));")

goal1 = "fMatch(Cons(A(), Cons(A(), Cons(B(), Nil()))), str)"
goal2 = "fMatch(Cons(A(), Cons(A(), Nil())), str)"
t2 goal = ( goal, --"fMatch(Cons(A(), Nil()), str)",
	" gEqSymb(A(), y) = gEqA(y);\
	\ gEqSymb(B(), y) = gEqB(y);\
	\ gEqA(A()) = True();  gEqA(B()) = False();\
	\ gEqB(A()) = False(); gEqB(B()) = True();\
	\ gIf(True(), x, y) = x;\
	\ gIf(False(), x, y) = y;\
	\ fMatch(p, s) = gM(p, s, p, s);\
	\ gM(Nil(), ss, op, os) = True();\
	\ gM(Cons(p, pp), ss, op, os) = gX(ss, p, pp, op, os);\
	\ gX(Nil(), p, pp,  op, os) = False();\
	\ gX(Cons(s, ss), p, pp,  op, os) = gIf(gEqSymb(p, s), gM(pp, ss, op, os), gN(os, op));\
	\ gN(Nil(), op) = False(); \
	\ gN(Cons(s, ss), op) = gM(op, ss, op, ss);")
	
t3 = ("gEven(fSqr(S(x)))",
	" gAdd(Z(), y) = y;\
	\ gAdd(S(x), y) = S(gAdd(x, y));\
	\ gMult(Z(), y) = Z();\
	\ gMult(S(x), y) = gAdd(y, gMult(x, y));\ 
	\ fSqr(x) = gMult(x, x); \
	\ gEq(Z(), y) = gEqZ(y);\
	\ gEq(S(x), y) = gEqS(y, x);\
	\ gEqZ(Z()) = True();\
	\ gEqZ(S(x)) = False();\
	\ gEqS(Z(), x) = False();\
	\ gEqS(S(y), x) = gEq(x, y);\
	\ gEven(Z()) = True();\
	\ gEven(S(x)) = gOdd(x);\
	\ gOdd(Z()) = False();\
	\ gOdd(S(x)) = gEven(x);")

t4 = ("gMult(x, S(Z()))",
	" gAdd(Z(), y) = y;\
	\ gAdd(S(x), y) = S(gAdd(x, y));\
	\ gMult(Z(), y) = Z();\
	\ gMult(S(x), y) = gAdd(gMult(x, y), y);\ 
	\ fSqr(x) = gMult(x, x); \
	\ gEq(Z(), y) = gEqZ(y);\
	\ gEq(S(x), y) = gEqS(y, x);\
	\ gEqZ(Z()) = True();\
	\ gEqZ(S(x)) = False();\
	\ gEqS(Z(), x) = False();\
	\ gEqS(S(y), x) = gEq(x, y);")

	
main = do
	putStrLn "just transformation"
	putStrLn (doTask transform (t2 goal1))
	
	putStrLn "deforestation"
	putStrLn (doTask deforest (t2 goal1))
		
	putStrLn "supercompilation"
	putStrLn (doTask supercompile (t2 goal1))
	
	--putStrLn "deforestation"
	--putStrLn (doTask deforest (t2 goal2))
	
	--putStrLn "supercompilation"
	--putStrLn (doTask supercompile (t2 goal2))
	
	-- this example shows that sometimes deforestation
	-- is better than supercompilation
	putStrLn "2) deforestation"
	putStrLn (doTask deforest t3)
	
	putStrLn "2) supercompilation"
	putStrLn (doTask supercompile t3)