module Inlining (inline) where

import Data
import Data.List

inline :: Program -> Program
inline (Program fDefs gDefs) = Program (filter (not . isFD0) simplifiedFDef) simplifiedGDef
  where
    fDefs0 = filter isFD0 fDefs

    simplifyFDef f =
      foldl (\(FDef n args body) sfd -> FDef n args (simplify sfd body)) f fDefs0
    simplifyGDef g =
      foldl (\(GDef n p args body) sfd -> GDef n p args (simplify sfd body)) g fDefs0

    simplifiedFDef = map simplifyFDef fDefs
    simplifiedGDef = map simplifyGDef gDefs

    isFD0 :: FDef -> Bool
    isFD0 (FDef _ args _) = null args 

simplify :: FDef -> Expr -> Expr
simplify fDef (Ctr   cn args) = Ctr   cn $ map (simplify fDef) args
simplify fDef@(FDef n [] body) (FCall cn _) | n == cn = body
simplify fDef (FCall cn args) = FCall cn $ map (simplify fDef) args
simplify fDef (GCall cn args) = GCall cn $ map (simplify fDef) args

simplify fDef (Let (n, e1) e2) = Let (n, simplify fDef e1) $ simplify fDef e2
simplify fDef (MultiLet   l e) =
  MultiLet (map (\(v, e) -> (v, simplify fDef e)) l) $ simplify fDef e
simplify _ e = e
