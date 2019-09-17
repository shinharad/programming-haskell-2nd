
-- ----------------------------------------------
-- 12.2 アプリ化ティブ

{--
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
--}

main1 = do
  print $ pure (+1) <*> Just 1
  print $ pure (+) <*> Just 1 <*> Just 2
  print $ pure (+) <*> Nothing <*> Just 2

main2 = do
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

main3 = do
  getChars' 10 >>= print
  --getChars 10 >>= print
