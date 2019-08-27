import Data.Char

-- ----------------------------------------------
-- 1.

main1 = do
  print $ sum [ x^2 | x <- [1..100]]

-- ----------------------------------------------
-- 2.

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0..m], y <- [0..n]]

-- ----------------------------------------------
-- 3.

square :: Int -> [(Int, Int)]
square n = [ (x, y) | (x, y) <- grid n n, x /= y]

-- ----------------------------------------------
-- 4.

replicate' :: Int -> a -> [a]
replicate' n x = [ x | _ <- [1..n]]

-- ----------------------------------------------
-- 5.

pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- ----------------------------------------------
-- 6.

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

-- ----------------------------------------------
-- 7.

reexpressed :: [(Int,Int)]
reexpressed = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- ----------------------------------------------
-- 8.

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = map snd xs'
  where
    xs' = filter (\(a, b) -> a == x) (zip xs [0..])

-- ----------------------------------------------
-- 9.

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- ----------------------------------------------
-- 10.

let2int :: Char -> Int
let2int c = ord c - ord 'a'

let2int' :: Char -> Int
let2int' c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2let' :: Int -> Char
int2let' n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2let' ((let2int' c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

main10 = do
  let encoded = encode 3 "HASKELL IS FUN"
  print $ encoded
  print $ encode (-3) encoded