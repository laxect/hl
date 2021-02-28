module Day1 (tri', showTri) where

tri = [(x, y, z) | x <- [1 ..], y <- [1 .. x], z <- [1 .. y], y ^ 2 + z ^ 2 == x ^ 2]

tri' = zip tri [0 ..]

showTri (x, y, z) = show x ++ ", " ++ show y ++ ", " ++ show z
