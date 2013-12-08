module Util (
    unique,
    closure,
) where

import qualified Data.Set as S

unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

closure :: Ord a => (S.Set a -> S.Set a) -> S.Set a -> S.Set a
closure step base = go S.empty base where
    go acc new = if S.null new then acc else go acc' new' where
        acc' = acc `S.union` new
        new' = step new `S.difference` acc'
