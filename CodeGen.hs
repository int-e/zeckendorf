{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module CodeGen (
    generate
) where

import Language.Haskell.Exts
import Language.Haskell.Exts.QQ
import Control.Applicative
import qualified Data.Set as S
import Automaton
import Util

-- | Generate code from an automaton.
--
-- The symbols are pairs @(Int,Maybe Int)@, where the first component
-- is an input value (e.g. 0..2 from addition), the second component
-- is an output character or 'Nothing' if we are at the end of output.
--
-- In case that the input list is exhausted, a 0 is appended.
--
-- The projection of the automaton to its first component is assumed to
-- be unambiguous. This ensures that the pattern match marked by !!! below
-- always succeeds.
generate :: Ord s => String -> Automaton s (Int,Maybe Int) -> [Decl]
generate fun aut =
    [[dec| __fun__ :: [Int] -> [Int] |],
     patch [dec| __fun__ = $(ci) where n f c = f . (c:) |]]
  where
    autF = snd . relabel . minimize . snd . relabel . determinize . trim $ aut
    (renP, autP) = relabel . determinize . project fst $ autF

    c i = "c" ++ show i; cn = Ident . c; cv = var . cn
    r i = "r" ++ show i; rn = Ident . r; rv = var . rn
    l = SrcLoc "<generated>" 1 1

    app = foldl App
    ci = app (cv i) [var (Ident "id") | _ <- S.toList $ renP i] where
        [i] = inits autP
    patch (PatBind l f t r (BDecls bs)) = PatBind l f t r (BDecls (bs ++ aux))

    aux = map (FunBind . stateFun) (S.toList $ states autP)

    stateFun s0 = [m0] ++ map step is where
        ss = S.toList (renP s0)
        match fun ps rhs = Match l fun ps Nothing (UnGuardedRhs rhs) (BDecls [])
        m0 = match (cn s0)
            ([[pat| __rs__ |] | s <- ss, let rs = r s] ++ [[pat| [] |]])
            rhs0
        rhs0
            | s0 `elem` finals autP = app (rv i) [[hs| [] |]]
            | otherwise = app (cv s0) $
                map rv ss ++ [[hs| [0] |]]
          where
            [i] = [s | s <- ss, s `elem` finals autF]
        is = unique [l | (s, l, s') <- trans autP, s == s0]
        step l = match (cn s0)
            ([[pat| __rs__ |] | s <- ss, let rs = r s] ++ [pxs])
            rhs
          where
            pxs = PInfixApp (intP (fromIntegral l)) (Special Cons) (pvar (Ident "xs"))
            [sn] = [s' | (s,l',s') <- trans autP, s == s0, l' == l]
            s = c sn
            rhs = app [hs| __s__ |] $
                [e | i <- S.toList (renP sn),
                 let [(r,o)] = (ri i), -- !!!
                 let e = case o of
                         Just o  -> [hs| n __r__ $o |]
                         Nothing -> [hs| __r__ |]
                 ] ++
                [[hs| xs |]]
            ri i = [(r j, intE . fromIntegral <$> o) |
                    (j,(l',o),i') <- trans autF,
                    i == i', l == l', j `S.member` renP s0]
