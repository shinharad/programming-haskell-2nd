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

