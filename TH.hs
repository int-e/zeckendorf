module TH (splice, display, addP, subP, addN, subN) where

import Language.Haskell.Meta
import Language.Haskell.Exts
import Control.Monad
import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
import Automaton
import CodeGen
import Util

makeAutomaton ::
    [Int] -> (Int -> Int -> Int -> Bool) -> (Int -> Int -> Bool) ->
    (Int -> Int -> Int -> Int -> Int) -> Automaton (Int,[Int],Int) (Int,Int)
makeAutomaton is check0 checkt out =
    Automaton{ inits = [(0,[],0)], finals = S.toList fs, trans = ts }
  where
    fs = closure (S.fromList . (>>= next) . S.toList) (S.singleton (0,[0,0,0],0))
    next s0 = [s | (s,(0,0),s') <- ts, s' == s0]
    ts = [(s,(i',o'),s') |
            s@(i,l,o) <- states,
            s'@(i',l',o') <- states,
            tail l == init l',
            checkt i i',
            o+o' <= 1] ++
        [((0,[],0),(i,o),s') |
            s'@(i,[c,d,e],o) <- states,
            check0 c d e]
    states = [(i,[c,d,e],o) |
            i <- is,
            [c,d,e] <- replicateM 3 [-1..1],
            let o = out i c d e,
            0 <= o && o <= 1]

splice :: [Decl] -> TH.Q [TH.Dec]
splice = return . toDecs

display :: [Decl] -> IO ()
display = mapM_ (putStrLn . prettyPrint)

addP, subP, addN, subN :: [Decl]
addP = generate "simpAdd" $ makeAutomaton [0..2] check0 checkt out where
    check0 c d e = c == -d
    out i c d e = i + c - d - e
    checkt i i' = i + i' <= 2

subP = generate "simpSub" $ makeAutomaton [-1..1] check0 checkt out where
    check0 c d e = c == -d
    out i c d e = i + c - d - e
    checkt i i' = i * i' /= 1

addN = generate "simpAdd" $ makeAutomaton [0..2] check0 checkt out where
    check0 c d e = c == 0
    out i c d e = i + c + d - e
    checkt i i' = i + i' <= 2

subN = generate "simpSub" $ makeAutomaton [-1..1] check0 checkt out where
    check0 c d e = c == 0
    out i c d e = i + c + d - e
    checkt i i' = i * i' /= 1

