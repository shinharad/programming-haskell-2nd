module Caesar where

import Data.Char

-- シーザー暗号

-- ----------------------------------------------
-- 5.5.1 暗号化と複合

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr $ ord 'a' + n

shift :: Int -> Char -> Char
shift n c | isLower c = int2let $ (let2int c + n) `mod` 26
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

main1 = do
  let encoded = encode 3 "haskell is fun"
  print $ encoded
  print $ encode (-3) encoded

-- ----------------------------------------------
-- 5.5.2 文字の出現頻度表
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

main2 = do
  print $ freqs "abbcccddddeeeee"

-- ----------------------------------------------
-- 5.5.3 暗号解読

-- カイ二乗検定
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

main3 = do
  print $ crack "kdvnhoo lv ixq"
    -- "haskell is fun"
  print $ crack "vscd mywzboroxcsyxc kbo ecopev"
    -- "list comprehensions are useful"

  -- ただし、文字列が短い場合や文字の出現頻度が例外的である場合には解読できない
  print $ crack (encode 3 "haskell")
    -- "piasmtt"
  print $ crack (encode 3 "boxing wizards jump quickly")
    -- "wjsdib rduvmyn ephk lpdxfgt"