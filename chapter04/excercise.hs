-- ----------------------------------------------
-- 1.
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (length xs `div` 2) xs

-- ----------------------------------------------
-- 2.
-- use head and tail
third :: [a] -> a
third xs = head . tail . tail $ xs

-- use !!
third' :: [a] -> a
third' xs = xs !! 2

-- use pattern matching
third'' :: [a] -> a
third'' (_:_:x:_) = x

-- ----------------------------------------------
-- 3.
-- condition
safetail :: [a] -> [a]
safetail xs =
    if null xs then
        []
    else
        tail xs

-- use gard
safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

-- use
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- ----------------------------------------------
-- 4.

{-
False || False = False
True || False = True
False || True = True
True || True = True

False || False = False
_ || _ = True

False || b = b
True || _ = True

b || c | b == c = b
       | otherwise = True
-}

-- ----------------------------------------------
-- 5.



