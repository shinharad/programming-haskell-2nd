module Countdown where

-- ----------------------------------------------
-- 9.2 算術演算子

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y  = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- ----------------------------------------------
-- 9.3 数式

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

main1 = do
  print $ App Add (Val 1) (App Mul (Val 2) (Val 3))
    -- 1+(2*3)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

main3 = do
  print $ eval (Val 1)
    -- [1]
  print $ eval (Val (-1))
    -- []
  print $ eval (App Add (Val 2) (Val 3))
    -- [5]
  print $ eval (App Sub (Val 2) (Val 3))
    -- []

-- ----------------------------------------------
-- 9.4 組み合わせ関数

-- リストの部分リストを返す
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- 新たな要素をリストへ挿入して返す
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- リストの要素に対する順列を返す
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- リストから選択肢を返す
choices :: [a] -> [[a]]
choices = concat . map perms . subs

main4 = do
  print $ subs [1,2,3]
    -- [],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
  print $ interleave 1 [2,3,4]
    -- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
  print $ perms [1,2,3]
    -- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
  print $ choices [1,2,3]
    -- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
    -- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
    -- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
    -- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-- ----------------------------------------------
-- 9.5 問題の形式化

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

main5 = do
  let e = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))
  print $ solution e [1,3,7,10,25,50] 765

-- ----------------------------------------------
-- 9.6 総当り法

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

main6 = do
  print $ split [1,2,3,4]
    -- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                       l <- exprs ls,
                       r <- exprs rs,
                       e <- combine l r] 

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- ----------------------------------------------
-- 9.7 性能テスト

{--
main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)
--}

{--
GHCiでは性能不足なのでGHCコンパイラを使用する

$ ghc -O2 countdown.hs
$ ./countdown
--}

-- ----------------------------------------------
-- 9.8 生成と評価の方法を変える

type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                         lx  <- results ls,
                         ry  <- results rs,
                         res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

{--
main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)
--}

-- ----------------------------------------------
-- 9.9 代数的な性質をいかす

{--
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
--}

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n, n) | n > 0]
results' ns  = [res | (ls,rs) <- split ns,
                         lx  <- results' ls,
                         ry  <- results' rs,
                         res <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns, (e,m) <- results' ns', m == n]

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)
