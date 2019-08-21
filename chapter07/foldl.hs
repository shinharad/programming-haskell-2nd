{--
sum :: Num a => [a] -> a
sum = sum' 0
  where
    sum' v [] = v
    sum' v (x:xs) = sum' (v+x) xs
--}

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

product' :: Num a => [a] -> a
product' = foldl (*) 1

or' :: [Bool] -> Bool
or' = foldl (||) False

and' :: [Bool] -> Bool
and' = foldl (&&) True

length' :: [a] -> Int
length' = foldl (\n _ -> n + 1) 0

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x:xs) []
