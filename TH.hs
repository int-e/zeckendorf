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
    (Int -> Int -> Int -> Int -> Int) ->
    Automaton (Int,[Int],Maybe Int) (Int,Maybe Int)
makeAutomaton vs check0 checkt out =
    Automaton{ inits = is, finals = fs, trans = ts }
  where
    is = [s | s@(i,[_,c,d],o) <- states, i == 0, val o == 0, check0 c d undefined]
    fs = [(0,[0,0,0],Nothing)]
    val = maybe 0 id
    ts = [(s,(i',o'),s') |
            s@(i,l,o) <- states,
            s'@(i',l',o') <- states,
            o /= Nothing || o' == Nothing,
            o /= Just 0 || o' /= Nothing,
            tail l == init l',
            checkt i i',
            val o + val o' <= 1]
    states = [(i,[c,d,e],o') |
            i <- vs,
            [c,d,e] <- replicateM 3 [-1..1],
            let o = out i c d e,
            0 <= o && o <= 1,
            o' <- [Nothing | o == 0] ++ [Just o]]

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

