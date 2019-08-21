-- ----------------------------------------------
-- 1.

ex1 :: [a] -> (a -> Bool) -> (a -> a) -> [a]
ex1 xs p f = map f $ filter p xs

main1 = do
  print $ ex1 [1,2,3,4,5,6,7] even (+1)

-- ----------------------------------------------
-- 2.
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                 = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

-- ----------------------------------------------
-- 3.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- ----------------------------------------------
-- 4.
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- ----------------------------------------------
-- 5.
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \a b -> f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a, b) -> f a b

