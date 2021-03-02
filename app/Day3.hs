module Day3 where

-- pattern matching

lucky :: (Integral a) => a -> String
lucky 7 = "7!"
lucky x = "what's this"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- original version
-- haskell's linter doesn't like this.
-- sum' :: (Num a) => [a] -> a
-- sum' [] = 0
-- sum' (x:xs) = x + sum' xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 1

head' :: String -> String
head' all@(h : xs) = [h] ++ "#" ++ all

-- guard
bmi :: (RealFloat a) => a -> String
bmi x
  | x <= 18.5 = "Oh!"
  | x <= 25 = "Ok!"
  | x <= 30 = "Oh!"
  | otherwise = "Oh!!!"

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2

day3 :: IO ()
day3 = do
  putStrLn "Day3!\n"
  print (lucky 7)
  print (lucky 0)
  print (factorial 4)
  print (addVectors (1, 2) (2, 3))
  print [a + b | (a, b) <- [(1, 2), (3, 4)]]
  print (sum' [1, 2, 3])
  print (head' "done")
  print (bmi 100)
  print (bmiTell 40 1.7)
