module Excercise where

import Data.Char

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

-- ----------------------------------------------
-- 6.

type Bit = Int 

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (null) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold (null) (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (const False) id f


-- ----------------------------------------------
-- 7.

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

transmit :: String -> String
transmit = decode . channel . encode

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

channel :: [Bit] -> [Bit]
channel = id

decode :: [Bit] -> String
decode =  map (chr . bin2int . check) . chop9
  where
    chop9 = unfold (null) (take 9) (drop 9)
    check (x:xs)
      | x == (calcParity xs) = xs
      | otherwise            = error "parity error!"

addParity :: [Bit] -> [Bit]
addParity bits = (calcParity bits) : bits

calcParity :: [Bit] -> Bit
calcParity bits = sum bits `mod` 2

main7 = do
  print $ transmit "higher-order functions are easy"

-- ----------------------------------------------
-- 8.

brokenChannel :: [Bit] -> [Bit]
brokenChannel  bits = tail bits

brokenTransmit :: String -> String
brokenTransmit = decode . brokenChannel . encode

main8 = do
  print $ brokenTransmit "higher-order functions are easy"

-- ----------------------------------------------
-- 9.

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g (x:[])   = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

main9 = do
  print $ altMap (+10) (+100) [0,1,2,3,4]

-- ----------------------------------------------
-- 10.

luhnDouble :: Int -> Int
luhnDouble x
  | n < 10    = n
  | otherwise = n - 9
  where n = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10 == 0

luhn' :: [Int] -> Bool
luhn' xs = (mod (sum $ f xs) 10) == 0
  where
    f = altMap luhnDouble id
