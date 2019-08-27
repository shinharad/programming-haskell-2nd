-- ----------------------------------------------
-- 1.

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

-- ----------------------------------------------
-- 2.

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- ----------------------------------------------
-- 3.

{--
(^) :: Int -> Int -> INt
m ^ 0 = 1
m ^ n = m * (m ^ (n - 1))
--}

-- ----------------------------------------------
-- 4.

euclid :: Int -> Int -> Int
euclid m n | m < n = euclid m (n - m)
           | m > n = euclid (m - n) n
           | m == n = m

-- ----------------------------------------------
-- 6.

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == True = and' xs
            | otherwise = False

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' n x | n == 0 = []
               | otherwise = x : replicate' (n - 1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) n | n == 0 = x
               | n > 0 = (!!!) xs (n - 1)

elm' :: Eq a => a -> [a] -> Bool
elm' _ [] = False
elm' n (x:xs) | n == x = True
              | otherwise = elm' n xs

-- ----------------------------------------------
-- 7.

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- ----------------------------------------------
-- 8.

halve :: [a] -> ([a],[a])
halve xs = ((take n xs), (drop n xs))
  where n  = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
  where (ys,zs) = halve xs

-- ----------------------------------------------
-- 9.

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 xs     = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs

