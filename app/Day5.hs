module Day5 (day5) where

data Future = Pending | Resolved | Aborted

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = abs $ (x2 - x1) * (y2 - y1)

data Point = Point Float Float deriving (Show)

data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = abs $ (x2 - x1) * (y2 - y1)

nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) dx dy = Circle' (Point (x + dx) (y + dy)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) dx dy = Rectangle' (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

data Person = Person
  { firstName :: String,
    lastName :: String
  }
  deriving (Show)

data Option a = None | Some a

data Vector a = Vector a a a deriving (Show, Eq, Read)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

data Youbi = Nichi | Ketsu | Ka | Sui | Muku | Kin | Tsu deriving (Show, Eq, Bounded, Enum, Read, Ord)

infixr 5 :-:

data LList a = Empty | a :-: (LList a) deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: LList a -> LList a -> LList a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

tree = foldr treeInsert EmptyTree [8, 6, 4, 1, 7, 3, 5]

class Yes a where
  yes :: a -> Bool

instance Yes (Tree a) where
  yes EmptyTree = False
  yes _ = True

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

day5 :: IO ()
day5 = do
  putStrLn "Day5!\n"
  print (surface $ Circle 0 0 100)
  print (surface $ Rectangle 0 200 100 100)
  print (Circle 0 0 5)
  print (map (Circle 0 0) [1 .. 5])
  print (surface' $ Rectangle' (Point 0 0) (Point 20 (-10)))
  print $ nudge (Rectangle' (Point 0 0) (Point 5 5)) 5 0
  print $ firstName (Person {firstName = "Haruhi", lastName = "Suzumiya"})
  print $ Vector 1 1 1 == Vector 1 1 1
  print (read "Vector 1 1 1" :: (Vector Float))
  print [Nichi .. Tsu]
  print $ succ Ka
  print $ 1 :-: 1 :-: 2 :-: Empty
  print $ (1 :-: 1 :-: Empty) .++ (2 :-: Empty)
  print tree
  print $ treeElem 8 tree
  print $ treeElem (-8) tree
  print $ yes tree
  print $ fmap (* 2) Nothing
  print $ fmap (* 2) (Just 10000)
  print $ fmap (* (-1)) tree
