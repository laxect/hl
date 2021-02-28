module Main where

import Day1

main :: IO ()

day1 = "Day1!\n" ++ showTri (fst (tri' !! 100)) ++ "\n"

main = putStrLn (day1 ++ "end")
