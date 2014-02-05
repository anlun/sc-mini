module HomeoEmbedding (isEmbedding) where

import Data

isEmbedding :: Expr -> Expr -> Bool
isEmbedding (Var _) (Var _) = True

isEmbedding (Ctr   n args) (Ctr   n' args') | n == n' && length args == length args' =
  and $ map (uncurry isEmbedding) $ zip args args'
isEmbedding (FCall n args) (FCall n' args') | n == n' && length args == length args' =
  and $ map (uncurry isEmbedding) $ zip args args'
isEmbedding (GCall n args) (GCall n' args') | n == n' && length args == length args' =
  and $ map (uncurry isEmbedding) $ zip args args'

isEmbedding e (Ctr   _ args) = any (isEmbedding e) args
isEmbedding e (FCall _ args) = any (isEmbedding e) args
isEmbedding e (GCall _ args) = any (isEmbedding e) args

isEmbedding _ _ = False
