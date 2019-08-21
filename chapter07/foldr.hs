{--
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs
--}

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

{--
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)
--}

{--
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
--}

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0



