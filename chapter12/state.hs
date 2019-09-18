module State where

-- ----------------------------------------------
-- 12.3.2 Stateモナド
-- 状態変換器（state transformer）

type State = Int

-- STは、引数として状態を取り、「次の状態」を生成して返す
-- この「次の状態」には、関数が実行されたことで状態に発生した更新内容が反映されている
-- 状態変換器の型を一般化して結果も返せるようにする

-- typeを使って宣言した型はクラスのインスタンスにはできないのでnewtypeで宣言する
-- Sは型を区別するための構成子
newtype ST a = S (State -> (a, State))
-- type ST a = State -> (a, State)

-- 関数適用のための関数。newtypeから引き剥がす
app :: ST a -> State -> (a, State)
app (S st) x = st x

-- STを関手にする
instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

-- STをアプリカティブにする
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
    let (f, s') = app stf s
        (x, s'') = app stx s' in (f x, s''))

-- STをMonadにする
instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


-- ----------------------------------------------
-- 12.3.3 木構造のラベル付け

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _)   n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l', n') = rlabel l n
                        (r', n'') = rlabel r n'

main1 =
  print $ fst (rlabel tree 0)
  -- Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

{--
rlabelの定義が複雑なので、
Tree a -> Int -> (Tree Int, Int) は、状態変換器を用いると、
Tree a -> ST (Tree Int) と書き換えられる
--}

-- 未使用の整数を得るために、現在の状態を返し、次の整数が次の状態となる状態変換器を定義する
fresh :: ST Int
fresh = S (\n -> (n, n + 1))

-- STはアプリカティブなので、ラベル付けの関数は、以下のようにアプリカティブスタイルで実現できる
-- g <$> x は、pure g <*> x のように振る舞う
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- STはモナドでもあるので、do表記を使った同等な定義も可能
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return (Leaf n) -- 冗長なのでアプリカティブスタイルの方が良さそう
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')
