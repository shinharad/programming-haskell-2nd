
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

