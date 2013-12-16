-- Simplification functions

-- Zeckendorf addition, manually optimized
simpAddM :: [Int] -> [Int]
simpAddM xs = c0 xs id where
    n f c = f . (c:)
    c0 []     a     = a []
    c0 (0:zs) a     = c1 zs (n a 0) (n a 1) (n a 1)
    c0 (1:zs) a     = c3 zs (n a 1) (n a 0) (n a 0)
    c0 (2:zs) a     = c4 zs (n a 0) (n a 1)
    c1 []     a b c = a []
    c1 (0:zs) a b c = c1 zs (n a 0) (n a 1) (n b 0)
    c1 (1:zs) a b c = c3 zs (n a 1) (n c 0) (n a 0)
    c1 (2:zs) a b c = c4 zs (n c 0) (n a 1)
    c2 []     a b c = a []
    c2 (0:zs) a b c = c1 zs (n a 0) (n a 1) (n b 1)
    c2 (1:zs) a b c = c3 zs (n a 1) (n c 0) (n a 0)
    c2 (2:zs) a b c = c4 zs (n c 0) (n a 1)
    c3 []     a b c = a []
    c3 (0:zs) a b c = c2 zs (n a 0) (n c 0) (n b 1)
    c3 (1:zs) a b c = c4 zs (n c 0) (n a 0)
    c3 (2:zs) a b c = c5 zs (n c 0) (n c 1)
    c4 []     a b   = a [1]
    c4 (0:zs) a b   = c3 zs (n a 1) (n b 0) (n a 0)
    c4 (1:zs) a b   = c4 zs (n b 0) (n a 1)
    c4 (2:zs) a b   = c5 zs (n b 0) (n b 1)
    c5 []     a b   = a [0,1]
    c5 (0:zs) a b   = c4 zs (n a 0) (n b 0)

-- Zeckendorf addition, generated
simpAddP :: [Int] -> [Int]
simpAddP = c0 id where
    n f c = f . (c :)
    c0 r0 [] = r0 []
    c0 r0 (0 : xs) = c3 (n r0 1) r0 (n r0 0) xs
    c0 r0 (1 : xs) = c6 (n r0 1) (n r0 0) xs
    c0 r0 (2 : xs) = c9 (n r0 0) (n r0 1) xs
    c1 r1 r2 r4 r5 [] = r4 []
    c1 r1 r2 r4 r5 (0 : xs) = c1 (n r2 0) (n r5 1) r4 (n r5 0) xs
    c1 r1 r2 r4 r5 (1 : xs) = c5 (n r1 0) (n r5 1) (n r5 0) xs
    c1 r1 r2 r4 r5 (2 : xs) = c9 (n r1 0) (n r5 1) xs
    c2 r1 r4 r5 r6 [] = r4 []
    c2 r1 r4 r5 r6 (0 : xs) = c1 (n r6 1) (n r5 1) r4 (n r5 0) xs
    c2 r1 r4 r5 r6 (1 : xs) = c5 (n r1 0) (n r5 1) (n r5 0) xs
    c2 r1 r4 r5 r6 (2 : xs) = c9 (n r1 0) (n r5 1) xs
    c3 r3 r4 r5 [] = r4 []
    c3 r3 r4 r5 (0 : xs) = c1 (n r3 0) (n r5 1) r4 (n r5 0) xs
    c3 r3 r4 r5 (1 : xs) = c5 (n r3 0) (n r5 1) (n r5 0) xs
    c3 r3 r4 r5 (2 : xs) = c9 (n r3 0) (n r5 1) xs
    c4 r6 r7 r8 [] = c4 r6 r7 r8 [0]
    c4 r6 r7 r8 (0 : xs) = c2 (n r6 1) r7 (n r7 0) (n r8 0) xs
    c4 r6 r7 r8 (1 : xs) = c8 (n r8 0) (n r7 0) xs
    c4 r6 r7 r8 (2 : xs) = c10 (n r8 1) (n r8 0) xs
    c5 r6 r7 r11 [] = c5 r6 r7 r11 [0]
    c5 r6 r7 r11 (0 : xs) = c2 (n r6 1) r7 (n r7 0) (n r11 0) xs
    c5 r6 r7 r11 (1 : xs) = c8 (n r11 0) (n r7 0) xs
    c6 r7 r10 [] = c6 r7 r10 [0]
    c6 r7 r10 (0 : xs) = c2 (n r10 1) r7 (n r7 0) (n r10 0) xs
    c6 r7 r10 (1 : xs) = c8 (n r10 0) (n r7 0) xs
    c7 r8 r9 [] = c7 r8 r9 [0]
    c7 r8 r9 (0 : xs) = c4 (n r8 0) (n r9 1) (n r9 0) xs
    c7 r8 r9 (1 : xs) = c8 (n r8 0) (n r9 1) xs
    c7 r8 r9 (2 : xs) = c10 (n r8 1) (n r8 0) xs
    c8 r9 r11 [] = c8 r9 r11 [0]
    c8 r9 r11 (0 : xs) = c4 (n r11 0) (n r9 1) (n r9 0) xs
    c8 r9 r11 (1 : xs) = c8 (n r11 0) (n r9 1) xs
    c9 r12 r13 [] = c9 r12 r13 [0]
    c9 r12 r13 (0 : xs) = c4 (n r13 0) (n r12 1) (n r12 0) xs
    c10 r14 r15 [] = c10 r14 r15 [0]
    c10 r14 r15 (0 : xs) = c7 (n r14 0) (n r15 0) xs

-- Zeckendorf subtraction, generated
simpSubP :: [Int] -> [Int]
simpSubP = c0 id where
    n f c = f . (c :)
    c0 r0 r1 r2 r9 [] = r9 []
    c0 r0 r1 r2 r9 (-1 : xs) = c15 r9 xs
    c0 r0 r1 r2 r9 (0 : xs) = c2 r0 (n r0 0) r9 r1 (n r2 0) xs
    c0 r0 r1 r2 r9 (1 : xs) = c10 r9 (n r2 1) r1 (n r2 0) xs
    c1 r1 r2 r3 r9 [] = r9 []
    c1 r1 r2 r3 r9 (-1 : xs) = c15 r9 xs
    c1 r1 r2 r3 r9 (0 : xs) = c5 (n r3 1) (n r3 0) r9 r1 (n r2 0) xs
    c1 r1 r2 r3 r9 (1 : xs) = c10 r9 (n r2 1) r1 (n r2 0) xs
    c2 r1 r2 r9 r10 r11 [] = r9 []
    c2 r1 r2 r9 r10 r11 (-1 : xs) = c0 (n r11 1) r10 (n r11 0) r9 xs
    c2 r1 r2 r9 r10 r11 (0 : xs) = c5 (n r11 1) (n r11 0) r9 r1 (n r2 0) xs
    c2 r1 r2 r9 r10 r11 (1 : xs) = c10 r9 (n r2 1) r1 (n r2 0) xs
    c3 r3 r5 r6 r9 [] = r9 []
    c3 r3 r5 r6 r9 (-1 : xs) = c15 r9 xs
    c3 r3 r5 r6 r9 (0 : xs) = c5 (n r3 1) (n r3 0) r9 r5 (n r5 0) xs
    c3 r3 r5 r6 r9 (1 : xs) = c11 r9 (n r6 0) r5 (n r5 0) xs
    c4 r4 r5 r9 [] = r9 []
    c4 r4 r5 r9 (-1 : xs) = c15 r9 xs
    c4 r4 r5 r9 (0 : xs) = c5 (n r4 1) (n r4 0) r9 r5 (n r5 0) xs
    c4 r4 r5 r9 (1 : xs) = c11 r9 (n r4 0) r5 (n r5 0) xs
    c5 r5 r6 r9 r10 r11 [] = r9 []
    c5 r5 r6 r9 r10 r11 (-1 : xs) = c0 (n r11 1) r10 (n r11 0) r9 xs
    c5 r5 r6 r9 r10 r11 (0 : xs) = c5 (n r11 1) (n r11 0) r9 r5 (n r5 0) xs
    c5 r5 r6 r9 r10 r11 (1 : xs) = c11 r9 (n r6 0) r5 (n r5 0) xs
    c6 r6 r7 r12 r14 r15 [] = r14 []
    c6 r6 r7 r12 r14 r15 (-1 : xs) = c1 r12 (n r12 0) (n r15 0) r7 xs
    c6 r6 r7 r12 r14 r15 (0 : xs) = c6 (n r12 0) r7 (n r15 1) r14 (n r15 0) xs
    c6 r6 r7 r12 r14 r15 (1 : xs) = c12 r7 (n r6 0) (n r15 1) (n r15 0) xs
    c7 r6 r7 r14 r15 r16 [] = r14 []
    c7 r6 r7 r14 r15 r16 (-1 : xs) = c3 (n r15 0) (n r16 1) (n r16 0) r7 xs
    c7 r6 r7 r14 r15 r16 (0 : xs) = c6 (n r16 1) r7 (n r15 1) r14 (n r15 0) xs
    c7 r6 r7 r14 r15 r16 (1 : xs) = c12 r7 (n r6 0) (n r15 1) (n r15 0) xs
    c8 r6 r9 r12 r15 [] = r9 []
    c8 r6 r9 r12 r15 (-1 : xs) = c1 r12 (n r12 0) (n r15 0) r9 xs
    c8 r6 r9 r12 r15 (0 : xs) = c8 (n r12 0) r9 (n r15 1) (n r15 0) xs
    c8 r6 r9 r12 r15 (1 : xs) = c12 r9 (n r6 0) (n r15 1) (n r15 0) xs
    c9 r7 [] = c9 r7 [0]
    c9 r7 (-1 : xs) = c15 r7 xs
    c9 r7 (0 : xs) = c9 r7 xs
    c9 r7 (1 : xs) = c9 r7 xs
    c10 r7 r12 r17 r18 [] = c10 r7 r12 r17 r18 [0]
    c10 r7 r12 r17 r18 (-1 : xs) = c1 r12 (n r12 0) (n r18 0) r7 xs
    c10 r7 r12 r17 r18 (0 : xs) = c6 (n r12 0) r7 (n r18 1) r17 (n r18 0) xs
    c10 r7 r12 r17 r18 (1 : xs) = c9 r7 xs
    c11 r7 r16 r17 r18 [] = c11 r7 r16 r17 r18 [0]
    c11 r7 r16 r17 r18 (-1 : xs) = c3 (n r18 0) (n r16 1) (n r16 0) r7 xs
    c11 r7 r16 r17 r18 (0 : xs) = c6 (n r16 1) r7 (n r18 1) r17 (n r18 0) xs
    c11 r7 r16 r17 r18 (1 : xs) = c9 r7 xs
    c12 r7 r16 r20 r21 [] = c12 r7 r16 r20 r21 [0]
    c12 r7 r16 r20 r21 (-1 : xs) = c3 (n r20 0) (n r16 1) (n r16 0) r7 xs
    c12 r7 r16 r20 r21 (0 : xs) = c7 (n r16 1) r7 r20 (n r20 0) (n r21 0) xs
    c12 r7 r16 r20 r21 (1 : xs) = c9 r7 xs
    c13 r7 r19 r20 [] = c13 r7 r19 r20 [0]
    c13 r7 r19 r20 (-1 : xs) = c3 (n r20 0) (n r19 1) (n r19 0) r7 xs
    c13 r7 r19 r20 (0 : xs) = c7 (n r19 1) r7 r20 (n r20 0) (n r19 0) xs
    c13 r7 r19 r20 (1 : xs) = c9 r7 xs
    c14 r8 [] = r8 []
    c14 r8 (-1 : xs) = c4 (n r8 0) (n r8 1) r8 xs
    c14 r8 (0 : xs) = c16 r8 (n r8 1) (n r8 0) xs
    c14 r8 (1 : xs) = c13 r8 (n r8 0) (n r8 1) xs
    c15 r9 [] = r9 []
    c15 r9 (-1 : xs) = c15 r9 xs
    c15 r9 (0 : xs) = c15 r9 xs
    c15 r9 (1 : xs) = c9 r9 xs
    c16 r9 r13 r15 [] = r9 []
    c16 r9 r13 r15 (-1 : xs) = c1 r13 (n r13 0) (n r15 0) r9 xs
    c16 r9 r13 r15 (0 : xs) = c8 (n r13 0) r9 (n r15 1) (n r15 0) xs
    c16 r9 r13 r15 (1 : xs) = c12 r9 (n r13 0) (n r15 1) (n r15 0) xs

-- Nega-Zeckendorf addition, generated
simpAddN :: [Int] -> [Int]
simpAddN = c0 id where
    n f c = f . (c :)
    c0 r0 r2 r3 r4 [] = c0 r0 r2 r3 r4 [0]
    c0 r0 r2 r3 r4 (0 : xs) = c4 r0 (n r2 0) r3 (n r3 0) (n r4 1) xs
    c0 r0 r2 r3 r4 (1 : xs) = c8 r0 (n r2 0) (n r3 0) xs
    c0 r0 r2 r3 r4 (2 : xs) = c11 (n r2 0) (n r2 1) xs
    c1 r0 r2 r10 r11 [] = r10 []
    c1 r0 r2 r10 r11 (0 : xs) = c3 r0 (n r2 0) (n r11 1) r10 (n r11 0) xs
    c1 r0 r2 r10 r11 (1 : xs) = c7 r0 (n r11 1) (n r2 0) xs
    c1 r0 r2 r10 r11 (2 : xs) = c11 (n r2 0) (n r2 1) xs
    c2 r1 [] = r1 []
    c2 r1 (0 : xs) = c5 r1 (n r1 0) (n r1 1) xs
    c2 r1 (1 : xs) = c7 r1 (n r1 1) (n r1 0) xs
    c2 r1 (2 : xs) = c11 (n r1 0) (n r1 1) xs
    c3 r5 r7 r9 r10 r11 [] = r10 []
    c3 r5 r7 r9 r10 r11 (0 : xs) = c3 r9 (n r9 0) (n r11 1) r10 (n r11 0) xs
    c3 r5 r7 r9 r10 r11 (1 : xs) = c7 r9 (n r11 1) (n r9 0) xs
    c3 r5 r7 r9 r10 r11 (2 : xs) = c10 (n r9 0) r5 (n r7 0) xs
    c4 r5 r7 r12 r13 r14 [] = c4 r5 r7 r12 r13 r14 [0]
    c4 r5 r7 r12 r13 r14 (0 : xs) = c1 r12 (n r13 0) r14 (n r14 0) xs
    c4 r5 r7 r12 r13 r14 (1 : xs) = c6 (n r13 0) (n r13 1) r12 xs
    c4 r5 r7 r12 r13 r14 (2 : xs) = c9 (n r13 1) r5 (n r7 0) xs
    c5 r6 r8 r9 [] = r6 []
    c5 r6 r8 r9 (0 : xs) = c3 r9 (n r9 0) (n r8 1) r6 (n r8 0) xs
    c5 r6 r8 r9 (1 : xs) = c7 r9 (n r8 1) (n r9 0) xs
    c5 r6 r8 r9 (2 : xs) = c10 (n r9 0) r6 (n r8 0) xs
    c6 r11 r15 r16 [] = c6 r11 r15 r16 [0]
    c6 r11 r15 r16 (0 : xs) = c3 r15 (n r15 0) (n r11 1) r16 (n r11 0) xs
    c6 r11 r15 r16 (1 : xs) = c7 r15 (n r11 1) (n r15 0) xs
    c7 r12 r14 r17 [] = c7 r12 r14 r17 [0]
    c7 r12 r14 r17 (0 : xs) = c1 r12 (n r17 0) r14 (n r14 0) xs
    c7 r12 r14 r17 (1 : xs) = c6 (n r17 0) (n r17 1) r12 xs
    c8 r12 r17 r18 [] = c8 r12 r17 r18 [0]
    c8 r12 r17 r18 (0 : xs) = c0 r12 (n r17 0) (n r18 1) (n r18 0) xs
    c8 r12 r17 r18 (1 : xs) = c6 (n r17 0) (n r17 1) r12 xs
    c9 r14 r19 r20 [] = c9 r14 r19 r20 [0]
    c9 r14 r19 r20 (0 : xs) = c1 r19 (n r20 0) r14 (n r14 0) xs
    c10 r18 r19 r20 [] = c10 r18 r19 r20 [0]
    c10 r18 r19 r20 (0 : xs) = c0 r19 (n r20 0) (n r18 1) (n r18 0) xs
    c11 r18 r21 [] = c11 r18 r21 [0]
    c11 r18 r21 (0 : xs) = c0 r21 (n r21 0) (n r18 1) (n r18 0) xs

-- Nega-Zeckendorf subtraction, generated
simpSubN :: [Int] -> [Int]
simpSubN = c0 id where
    n f c = f . (c :)
    c0 r0 r1 r3 [] = c0 r0 r1 r3 [0]
    c0 r0 r1 r3 (0 : xs) = c9 r0 (n r1 0) (n r3 1) (n r3 0) xs
    c0 r0 r1 r3 (1 : xs) = c11 r0 (n r1 0) (n r3 1) xs
    c1 r2 r3 [] = c1 r2 r3 [0]
    c1 r2 r3 (0 : xs) = c9 r2 (n r2 0) (n r3 1) (n r3 0) xs
    c1 r2 r3 (1 : xs) = c11 r2 (n r2 0) (n r3 1) xs
    c2 r2 r8 r9 r13 [] = c2 r2 r8 r9 r13 [0]
    c2 r2 r8 r9 r13 (-1 : xs) = c3 r8 (n r9 0) (n r13 1) xs
    c2 r2 r8 r9 r13 (0 : xs) = c9 r2 (n r2 0) (n r9 1) (n r9 0) xs
    c2 r2 r8 r9 r13 (1 : xs) = c11 r2 (n r2 0) (n r9 1) xs
    c3 r4 r5 r6 [] = c3 r4 r5 r6 [0]
    c3 r4 r5 r6 (0 : xs) = c6 (n r5 0) r4 (n r6 0) xs
    c3 r4 r5 r6 (1 : xs) = c10 (n r5 0) (n r5 1) xs
    c4 r4 r17 r18 [] = c4 r4 r17 r18 [0]
    c4 r4 r17 r18 (-1 : xs) = c1 (n r17 1) (n r17 0) xs
    c4 r4 r17 r18 (0 : xs) = c5 (n r18 0) r4 (n r17 1) (n r17 0) xs
    c5 r5 r10 r12 r13 [] = r10 []
    c5 r5 r10 r12 r13 (-1 : xs) = c3 r12 (n r12 0) (n r13 1) xs
    c5 r5 r10 r12 r13 (0 : xs) = c6 (n r5 0) r10 (n r12 0) xs
    c5 r5 r10 r12 r13 (1 : xs) = c10 (n r5 0) (n r5 1) xs
    c6 r5 r10 r17 [] = r10 []
    c6 r5 r10 r17 (-1 : xs) = c1 (n r17 1) (n r17 0) xs
    c6 r5 r10 r17 (0 : xs) = c5 (n r5 0) r10 (n r17 1) (n r17 0) xs
    c6 r5 r10 r17 (1 : xs) = c10 (n r5 0) (n r5 1) xs
    c7 r7 [] = r7 []
    c7 r7 (-1 : xs) = c1 (n r7 1) (n r7 0) xs
    c7 r7 (0 : xs) = c8 r7 (n r7 0) (n r7 1) xs
    c7 r7 (1 : xs) = c10 (n r7 0) (n r7 1) xs
    c8 r10 r11 r12 [] = r10 []
    c8 r10 r11 r12 (-1 : xs) = c3 r12 (n r12 0) (n r11 1) xs
    c8 r10 r11 r12 (0 : xs) = c6 (n r11 0) r10 (n r12 0) xs
    c8 r10 r11 r12 (1 : xs) = c10 (n r11 0) (n r11 1) xs
    c9 r14 r15 r16 r17 [] = c9 r14 r15 r16 r17 [0]
    c9 r14 r15 r16 r17 (-1 : xs) = c1 (n r17 1) (n r17 0) xs
    c9 r14 r15 r16 r17 (0 : xs) = c5 (n r16 0) r16 (n r17 1) (n r17 0) xs
    c9 r14 r15 r16 r17 (1 : xs) = c4 r14 (n r16 0) (n r15 0) xs
    c10 r17 r19 [] = c10 r17 r19 [0]
    c10 r17 r19 (-1 : xs) = c1 (n r17 1) (n r17 0) xs
    c10 r17 r19 (0 : xs) = c5 (n r19 0) r19 (n r17 1) (n r17 0) xs
    c11 r20 r21 r22 [] = c11 r20 r21 r22 [0]
    c11 r20 r21 r22 (-1 : xs) = c0 r20 (n r21 0) (n r22 0) xs
    c11 r20 r21 r22 (0 : xs) = c2 (n r21 1) r20 (n r21 0) (n r22 0) xs
