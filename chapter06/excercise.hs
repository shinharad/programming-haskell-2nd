-- ----------------------------------------------
-- 1.
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

-- 2.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3.
{--
(^) :: Int -> Int -> INt
m ^ 0 = 1
m ^ n = m * (m ^ (n - 1))
--}

-- 4.
euclid :: Int -> Int -> Int
euclid m n | m < n = euclid m (n - m)
           | m > n = euclid (m - n) n
           | m == n = m

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
