module Main where

import Day1
import Day2

main :: IO ()

day1 = "Day1!\n\n" ++ showTri (fst (tri' !! 100))

main = do
  putStrLn day1
  putStrLn "\n==="
  day2
  putStrLn "\n==="
  putStrLn "end"
