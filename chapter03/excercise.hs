-- ----------------------------------------------
-- 1.

a1 = ['a','b','c'] :: [Char]

a2 = ('a','b','c') :: (Char, Char, Char)

a3 = [(False,'0'),(True,'1')] :: [(Bool, Char)]

a4 = ([False,True],['0','1']) :: ([Bool], [Char])

a5 = [tail, init, reverse] :: [[a] -> [a]]

-- ----------------------------------------------
-- 2.
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a

-- ----------------------------------------------
-- 3.
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num n => n -> n
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)