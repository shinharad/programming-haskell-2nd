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
