-- 3.
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- 4.
last' :: [Int] -> Int
last' = head . reverse

-- 5.
init' :: [Int] -> [Int]
init' = reverse . tail . reverse