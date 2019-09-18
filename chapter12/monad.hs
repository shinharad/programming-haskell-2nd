module Monad where

-- ----------------------------------------------
-- 12.1 関手

{--
inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns
--}

{--
inc = map (+1)

sqr = map (^2)
--}

{--
class Functor f where
  fmap :: (a -> b) -> f a -> fb
--}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

main1 = do
  print $ fmap length (Leaf "abc")
  print $ fmap even (Node (Leaf 1) (Leaf 2))

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

main2 = do
  print $ inc (Just 1)
  print $ inc [1,2,3,4,5]
  print $ inc (Node (Leaf 1) (Leaf 2))

-- ----------------------------------------------
-- 12.2 アプリカティブ

{--
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
--}

main3 = do
  print $ pure (+1) <*> Just 1
  print $ pure (+) <*> Just 1 <*> Just 2
  print $ pure (+) <*> Nothing <*> Just 2

main4 = do
  print $ pure (+1) <*> [1,2,3]
  print $ pure (+) <*> [1] <*> [2]
  print $ pure (*) <*> [1,2] <*> [3,4]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

-- 指定した文字数を打ったら即座に結果を返す
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

{--
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

*Main> :t replicate 10 getChar
replicate 10 getChar :: [IO Char]
--}

getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)

main5 = do
  getChars' 10 >>= print
  --getChars 10 >>= print

-- ----------------------------------------------
-- 12.3 モナド

data Expr = Val Int | Div Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

main6 = do
  print $ eval (Div (Val 1) (Val 0))

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

-- without monad
eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n  -> case eval' y of
                                 Nothing -> Nothing
                                 Just m -> safediv n m

main7 = do
  print $ eval' (Div (Val 1) (Val 0))

-- with bind
eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m ->
                   safediv n m

eval''' :: Expr -> Maybe Int
eval''' (Val n) = Just n
eval''' (Div x y) = do n <- eval''' x
                       m <- eval''' y
                       safediv n m
