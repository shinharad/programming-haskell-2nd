module Excercise where

-- 1.
double :: Int -> Int
double n = n + n

-- 3.
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

-- 4.
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

qsort' :: [Int] -> [Int]
qsort' = reverse . qsort 

-- 5.
qsort'' :: Ord a => [a] -> [a]
qsort'' [] = []
qsort'' (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]
