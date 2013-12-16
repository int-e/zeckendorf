module TH (splice, display, addP, subP, addN, subN) where

import Language.Haskell.Meta
import Language.Haskell.Exts
import Control.Monad
import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
import Automaton
import CodeGen
import Util

-- Make an automaton that accepts matching input/output sequences,
-- where output sequences are valid Zeckendorf strings, and the two
-- strings can be related by carries.
--
-- States are (i,[c,d,e],o), where i is an input value; o is Nothing
-- (denoting the end of the string), Just 0, or Just 1. [c,d,e] is
-- a pattern of carries. While states encode the arithmetic (how
-- carries are added), the transitions encode the logic by which
-- carries are propagated.

makeAutomaton ::
    [Int] -> (Int -> Int -> Bool) -> (Int -> Int -> Bool) ->
    (Int -> Int -> Int -> Int -> Int) ->
    Automaton (Int,[Int],Maybe Int) (Int,Maybe Int)
makeAutomaton vs check0 checkt out =
    Automaton{ inits = is, finals = fs, trans = ts }
  where
    is = [s | s@(i,[_,c,d],o) <- states, i == 0, val o == 0, check0 c d]
    fs = [(0,[0,0,0],Nothing)]
    val = maybe 0 id
    ts = [(s,(i',o'),s') |
            s@(i,l,o) <- states,
            s'@(i',l',o') <- states,
            o /= Nothing || o' == Nothing, -- no output after end of string
            o /= Just 0 || o' /= Nothing,  -- no trailing zeros
            tail l == init l',             -- match carries
            checkt i i',                   -- check that i' may follow i
            val o + val o' <= 1]           -- check that o' may follow o
    states = [(i,[c,d,e],o') |
            i <- vs,
            [c,d,e] <- replicateM 3 [-1..1],
            let o = out i c d e,           -- compute o from i and carries
            0 <= o && o <= 1,
            o' <- [Nothing | o == 0] ++ [Just o]]

splice :: [Decl] -> TH.Q [TH.Dec]
splice = return . toDecs

display :: [Decl] -> IO ()
display = mapM_ (putStrLn . prettyPrint)

addP, subP, addN, subN :: [Decl]
addP = generate "simpAdd" $ makeAutomaton [0..2] check0 checkt out where
    check0 c d = c == -d
    out i c d e = i + c - d - e
    checkt i i' = i + i' <= 2

subP = generate "simpSub" $ z $ makeAutomaton [-1..1] check0 checkt out where
    check0 c d = c == -d
    out i c d e = i + c - d - e
    checkt i i' = i * i' /= 1
    -- To make 'sub' total, we add an automaton that recognizes
    -- inputs from {-1,0,1}^*{-1}{0}^*, returning the empty string.
    z aut@Automaton{ inits = is, finals = fs, trans = ts } =
        aut{ inits = is ++ is', finals = fs ++ fs', trans = ts ++ ts' }
      where
        s0 = (0,[],Nothing)
        s1 = (1,[],Nothing)
        is' = [s0]
        fs' = [s1]
        ts' = [(s0,(i,Nothing),s0) | i <- [-1..1]] ++
              [(s0,(-1,Nothing),s1),(s1,(0,Nothing),s1)]

addN = generate "simpAdd" $ makeAutomaton [0..2] check0 checkt out where
    check0 c d = c == 0
    out i c d e = i + c + d - e
    checkt i i' = i + i' <= 2

subN = generate "simpSub" $ makeAutomaton [-1..1] check0 checkt out where
    check0 c d = c == 0
    out i c d e = i + c + d - e
    checkt i i' = i * i' /= 1

