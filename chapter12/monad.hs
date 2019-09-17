

-- ----------------------------------------------
-- 12.3 モナド

data Expr = Val Int | Div Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

main1 = do
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

main2 = do
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