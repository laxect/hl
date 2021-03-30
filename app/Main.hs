module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8

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
  day6
  putStrLn "\n==="
  day7
  putStrLn "\n==="
  day8
  putStrLn "\n==="
  putStrLn "end"
