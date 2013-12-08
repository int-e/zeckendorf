module Automaton (
    Automaton (..),
    states,
    labels,
    reachable,
    productive,
    trim,
    determinize,
    minimize,
    project,
    relabel,
) where

import qualified Data.Set as S
import qualified Data.Map as M
import Util

data Automaton s l = Automaton {
    inits  :: [s],
    finals :: [s],
    trans  :: [(s,l,s)]
} deriving Show

states :: Ord s => Automaton s l -> S.Set s
states aut = S.fromList $ inits aut ++ finals aut ++
             [s'' | (s,_,s') <- trans aut, s'' <- [s,s']]

labels :: Ord l => Automaton s l -> S.Set l
labels aut = S.fromList [l | (_,l,_) <- trans aut]

reachable :: Ord s => Automaton s l -> S.Set s
reachable aut = closure step (S.fromList (inits aut)) where
    step new = S.fromList [s' | (s,l,s') <- trans aut, s `S.member` new]

productive :: Ord s => Automaton s l -> S.Set s
productive aut = closure step (S.fromList (finals aut)) where
    step new = S.fromList [s | (s,l,s') <- trans aut, s' `S.member` new]

trim :: Ord s => Automaton s l -> Automaton s l
trim aut = aut{ trans = trans' } where
    useful = reachable aut `S.intersection` productive aut
    trans' = [t | t@(s,l,s') <- trans aut,
              s `S.member` useful, s' `S.member` useful]

relabel :: (Ord s, Integral a) => Automaton s l -> (a -> s, Automaton a l)
relabel aut =
    (ren', aut{ inits  = map ren (inits aut),
                finals = map ren (finals aut),
                trans  = [(ren s,l,ren s') | (s,l,s') <- trans aut] })
  where
    ren  s = M.fromList (zip (S.toList (states aut)) [0..]) M.! s
    ren' a = M.fromList (zip [0..] (S.toList (states aut))) M.! a

determinize :: (Ord s, Ord l) => Automaton s l -> Automaton (S.Set s) l
determinize aut = aut{ inits = inits', finals = finals', trans = trans' } where
    states' = S.toList $ closure
        (S.fromList . concatMap (M.elems . next) . S.toList)
        (S.singleton . S.fromList $ inits aut)
    next ss = M.fromListWith S.union
        [(l, S.singleton s') | (s,l,s') <- trans aut, s `S.member` ss]
    inits'  = [S.fromList (inits aut)]
    finals' = [s | s <- states', any (`S.member` s) (finals aut)]
    trans'  = [(s,l,s') | s <- states', (l,s') <- M.toList $ next s]

minimize :: (Ord s, Ord l) => Automaton s l -> Automaton (S.Set s) l
minimize aut = aut{ inits = inits', finals = finals', trans = trans' } where
    i = S.fromList $ finals aut
    states' = subdiv [i, states aut `S.difference` i]
    subdiv xs = if length xs == length xs' then xs' else subdiv xs' where
        xs' = foldl (\xs p -> xs >>= \s -> filter (not . S.null)
            [s `S.intersection` p, s `S.difference` p]) xs (xs >>= pre)
    pre ss = M.elems $ M.fromListWith S.union
        [(l, S.singleton s) | (s,l,s') <- trans aut, s' `S.member` ss]
    inits'  = [s' | s' <- states', any (`S.member` s') (inits aut)]
    finals' = [s' | s' <- states', any (`S.member` s') (finals aut)]
    trans'  = unique [(ss, l, ss') | (s,l,s') <- trans aut,
        ss <- states', s `S.member` ss, ss' <- states', s' `S.member` ss']

project :: (l -> l') -> Automaton s l -> Automaton s l'
project f aut = aut{ trans = [(s, f l, s') | (s, l, s') <- trans aut] }
