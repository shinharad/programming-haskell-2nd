module Memo where

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- 8.6 恒真式検査器

data Prop = Const Bool 
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop
            | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = (Var 'A') `And` (Not (Var 'A'))

p6 :: Prop
p6 = (Var 'A' `And` (Var 'A' `Imply` Var 'B')) `Imply` Var 'B'

p7 :: Prop
p7 = (Var 'A') `Imply` ((Var 'A') `Or` (Var 'B'))

p8 :: Prop
p8 = (Var 'B') `Imply` ((Var 'A') `Or` (Var 'B'))

p9 :: Prop
p9 = (((Var 'A') `Or` (Var 'B'))) `Equiv` ((Var 'B') `Or` (Var 'A'))

p10 :: Prop
p10 = (((Var 'A') `And` (Var 'B'))) `Equiv` ((Var 'B') `And` (Var 'A'))

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
           where bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : (rmdups $ filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- 8.7 抽象機械

data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval' :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int 
exec [] n           = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c) m  = exec c (n + m)

value' :: Expr -> Int
value' e = eval' e []
