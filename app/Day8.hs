module Day8 (day8) where

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

day8 = do
  putStrLn "Day8!"
  print $ fmap (++ "ha") (CJust 0 "ho")
  -- not follow functor law
  print $ fmap (++ "he") (fmap (++ "ha") (CJust 0 "ho"))
  print $ fmap ((++ "he") . (++ "ha")) (CJust 0 "ho")
  print $ fmap id (CJust 0 "ho")
  -- applcative functor
  print $ [(* 0), (+ 2)] <*> [1, 2]
  print $ (*) <$> [2, 5, 10] <*> [8, 10, 11]
