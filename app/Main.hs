module Main where

import Day1
import Day2
import Day3
import Day4
import Day5

main :: IO ()

day1 = "Day1!\n\n" ++ showTri (fst (tri' !! 100))

main = do
  putStrLn day1
  putStrLn "\n==="
  day2
  putStrLn "\n==="
  day3
  putStrLn "\n==="
  day4
  putStrLn "\n==="
  day5
  putStrLn "\n==="
  putStrLn "end"
