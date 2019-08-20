-- 7.6 文字列の２進数変換器

import Data.Char

-- ----------------------------------------------
-- 7.6.2 基数変数

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1

{--
*Main> :t iterate
iterate :: (a -> a) -> a -> [a]
--}

main1 = do
  print $ take 5 $ iterate (*2) 1
    -- [1,2,4,8,16]
  print $ bin2int [1,0,1,1]
    -- 13

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

main2 = do
  print $ bin2int' [1,0,1,1]

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

main3 = do
  print $ int2bin 13
    -- [1,0,1,1]

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

main4 = do
  print $ make8 [1,0,1,1]
    -- [1,0,1,1,0,0,0,0]

-- ----------------------------------------------
-- 7.6.3 通信

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

{--
*Main> :t ord
ord :: Char -> Int

*Main> :t concat
concat :: Foldable t => t [a] -> [a]

*Main> :t map (make8 . int2bin . ord)
map (make8 . int2bin . ord) :: [Char] -> [[Bit]]
--}

main5 = do
  print $ encode "abc"
    -- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

{--
*Main> :t chr
chr :: Int -> Char
--}

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

main6 = do
  print $ decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
    -- "abc"

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

main7 = do
  print $ transmit "higher-order functions are easy"
    -- "higher-order functions are easy"


