module Prototype where
	
import Data
import DataUtil
import Driving
import Folding
import Generator
import Data.List
import HomeoEmbedding

transform :: Task -> Task
transform (e, p) =
	residuate $ foldTree $ buildFTree (driveMachine p) e

buildFTree :: Machine Conf -> Conf -> Tree Conf
buildFTree m e = bft [] m nameSupply e

bft :: [Expr] -> Machine Conf -> NameSupply -> Conf -> Tree Conf
bft el d ns e | homeoWhistle e el = uncurry (bft (e:el) d) (fullGeneralize ns e)
--bft el d ns e | whistle e = uncurry (bft el d) (fullGeneralize ns e)
--bft d ns e | whistle e = uncurry (bft d) (fullGeneralize ns e)
--bft d (n:ns) e | whistle e = bft d ns $ generalize n e
bft el d ns t | otherwise = case d ns t of
	Decompose ds -> Node t $ Decompose $ map (bft (t:el) d ns) ds
	Transient e -> Node t $ Transient $ bft (t:el) d ns e
	Stop -> Node t Stop
	Variants cs -> Node t $ Variants [(c, bft (t:el) d (unused c ns) e) | (c, e) <- cs]

sizeBound = 40
whistle :: Expr -> Bool
whistle e@(FCall _ args) = not (all isVar args) && size e > sizeBound
whistle e@(GCall _ args) = not (all isVar args) && size e > sizeBound
whistle _ = False

homeoWhistle :: Expr -> [Expr] -> Bool
homeoWhistle e = any (\e' -> isEmbedding e' e)

fullGeneralize :: NameSupply -> Expr -> (NameSupply, Expr)
fullGeneralize nl (FCall f es) = (nl', MultiLet (zip nl es) (FCall f vl)) where
    (vars, nl') = splitAt (length es) nl
    vl = map (Var) vars

fullGeneralize nl (GCall f es) = (nl', MultiLet (zip nl es) (GCall f vl)) where
    (vars, nl') = splitAt (length es) nl
    vl = map (Var) vars

generalize :: Name -> Expr -> Expr
generalize n (FCall f es) = 
	Let (n, e) (FCall f es') where (e, es') = extractArg n es
generalize n (GCall g es) = 
	Let (n, e) (GCall g es') where (e, es') = extractArg n es

extractArg :: Name -> [Expr] -> (Expr, [Expr])	
extractArg n es = (maxE, vs ++ Var n : ws) where
	maxE = maximumBy ecompare es
	ecompare x y = compare (eType x * size x) (eType y * size y)
	(vs, w : ws) = break (maxE ==) es
	eType e = if isVar e then 0 else 1
