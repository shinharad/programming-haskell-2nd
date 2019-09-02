module Excercise where

-- 1.

data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

main1 = do
  print $ add Zero Zero
    -- Zero
  print $ add Zero (Succ (Succ Zero))
    -- Succ (Succ Zero)
  print $ add (Succ (Succ Zero)) (Succ Zero)
    -- Succ (Succ (Succ Zero))

  print $ mult Zero Zero
    -- Zero
  print $ mult Zero (Succ (Succ Zero))
    -- Zero
  print $ mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
    -- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

-- ----------------------------------------------
-- 2.

data Tree a = Leaf a | Node (Tree a) a (Tree a)

{--
# 元の定義
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y = True
                      | x < y = occurs x l
                      | otherwise = occurs x r
--}

-- 評価は compare x y の1回だけ
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r

-- ----------------------------------------------
-- 3.

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
  deriving Show

-- Leafの数を返す関数
countLeaves :: Tree' a -> Int
countLeaves (Leaf' _) = 1
countLeaves (Node' l r) = countLeaves l + countLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (countLeaves l - countLeaves r) <= 1
                      && balanced l && balanced r

main3 = do
  let data1 = (Leaf' 1)
  let data2 = Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))
  let data3 = Node' (Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))) (Leaf' 5)
  print $ countLeaves data1
  print $ countLeaves data2
  print $ countLeaves data3

  print $ balanced data1
  print $ balanced data2
  print $ balanced data3

-- ----------------------------------------------
-- 4.

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance ys) (balance zs)
  where (ys, zs) = halve xs

main4 = do
  let data1 = [1,2,3,4,5,6,7,8]
  print $ halve data1
  print $ balance data1

-- ----------------------------------------------
-- 5.

data Expr' = Val' Int | Add' Expr' Expr' deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr' -> a
folde f g (Val' x) = f x
folde f g (Add' x y) = g (folde f g x) (folde f g y)

-- ----------------------------------------------
-- 6.

eval' :: Expr' -> Int
eval' x = folde id (+) x

size :: Expr' -> Int
size x = folde (\_ -> 1) (+) x

main6 = do
  let data1 = (Add' (Val' 1) (Val' 2))
  let data2 = (Add' (Add' (Val' 1) (Val' 2)) (Val' 3))
  print $ eval' data1
  print $ eval' data2
  print $ size data1
  print $ size data2

-- ----------------------------------------------
-- 7.

{--
instance Eq a => Eq (Maybe a) where
  Nothing  == Nothing  = True
  (Just x) == (Just y) = x == y
  _        == _        = False

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = x == y && xs == ys
  xs     == ys     = False
--}

