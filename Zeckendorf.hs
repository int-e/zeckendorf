{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Zeckendorf where

import Test.QuickCheck
import TH

-- Zeckendorf numbers
type Z = [Int]

-- Conversion
val :: Integral a => Z -> a
val zs = go 0 1 1 zs where
    go !n a b (z:zs) | z < 0 || z > 1 = error "invalid: digit out of range"
    go !n a b [0] = error "invalid: trailing zero"
    go !n a b (1:1:zs) = error "invalid: consecutive ones"
    go !n a b (z:zs) = go (fromIntegral z * b + n) b (a + b) zs
    go !n _ _ _      = n

zek :: Integral a => a -> Z
zek n = fst (go 1 1 n) where
    go a b n
        | b > n = ([],n)
    go a b n =
        let (zs, n') = go b (a + b) n in
        if b <= n' then (1:zs, n' - b) else (0:zs, n')

-- Arithmetic
inc :: Z -> Z
inc []       = [1]
inc (1:xs)   = 0:inc xs
inc (0:0:xs) = 1:0:xs
inc (0:1:xs) = 0:inc (1:xs)

$(splice addP)

add :: Z -> Z -> Z
add xs ys = simpAdd (add0 xs ys) where
    add0 []     ys     = ys
    add0 xs     []     = xs
    add0 (x:xs) (y:ys) = (x+y) : add0 xs ys

$(splice subP)

sub :: Z -> Z -> Z
sub xs ys = simpSub (sub0 xs ys) where
    sub0 []     ys     = map negate ys
    sub0 xs     []     = xs
    sub0 (x:xs) (y:ys) = (x-y) : sub0 xs ys

mul :: Z -> Z -> Z
mul xs = go [] xs xs where
    go n xs xs' (0:ys) = go n             xs' (xs `add` xs') ys
    go n xs xs' (1:ys) = go (n `add` xs') xs' (xs `add` xs') ys
    go n _  _   _      = n


-- Tests
prop0 a   = a >= 0           ==> val (zek a) == (a :: Integer)
prop1 a b = a >= 0 && b >= 0 ==> val (add (zek a) (zek b)) == (a + b :: Integer)
prop2 a b = a >= b && b >= 0 ==> val (sub (zek a) (zek b)) == (a - b :: Integer)
prop3 a b = a >= 0 && b >= 0 ==> val (mul (zek a) (zek b)) == (a * b :: Integer)
