module Day4 (day4) where

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) = quicksort' (filter (<= x) xs) ++ [x] ++ quicksort' (filter (> x) xs)

day4 :: IO ()
day4 = do
  putStrLn "Day4!\n"
  putStrLn (applyTwice (++ "c") "b")
  print (zipWith' (-) [1, 2, 3] [2, 3])
  print (flip' (-) 2 1)
  print (map (+ 4) [1, 4, 5])
  print (quicksort [3, 4, 2, 1])
  print (quicksort' [0, 3, 4, 2, 1])
  print (zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5])
  print $ (quicksort' . applyTwice (++ [2])) [1, 3]
