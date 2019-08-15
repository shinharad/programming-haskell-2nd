-- 1.
main1 = do
  print $ sum [ x^2 | x <- [1..100]]

-- 2.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0..m], y <- [0..n]]

-- 3.
square :: Int -> [(Int, Int)]
square n = [ (x, y) | (x, y) <- grid n n, x /= y]

-- 4.
replicate' :: Int -> a -> [a]
replicate' n x = [ x | _ <- [1..n]]

-- 5.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


-- 8.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = map snd xs'
  where
    xs' = filter (\(a, b) -> a == x) (zip xs [0..])


