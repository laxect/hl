module Day2 (day2) where

removeUpper :: [Char] -> [Char]
removeUpper str = [ch | ch <- str, ch `elem` ['A' .. 'Z']]

addTwo :: Int -> Int -> Int
addTwo x y = x + y

contain :: (Eq a) => a -> [a] -> Bool
contain x y = not (null [z | z <- y, z == x])

day2 :: IO ()
day2 = do
  putStrLn "Day2!\n"
  putStrLn (removeUpper "AaBbZz")
  print (addTwo 1 2)
  print ((==) 1 1)
  print (contain 1 [1, 4])
  print (read "True" :: Bool)
