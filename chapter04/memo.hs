module Memo where

-- ガード付きの等式
abs' :: Int -> Int
abs' n | n >= 0 = n
       | otherwise = -n

signum' :: Int -> Int
signum' n | n < 0 = -1
          | n == 0 = 0
          | otherwise = 1

