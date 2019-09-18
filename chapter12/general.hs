module General where

import Control.Monad
import Data.Char

-- ----------------------------------------------
-- 12.3.4 汎用的な関数

{--
> :t mapM
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
--}

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c) 
       | otherwise = Nothing

main1 = do
  print $ mapM conv "1234"
    -- Just [1,2,3,4]
  print $ mapM conv "123a"
    -- Nothing

{--
> :t filterM
filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
--}

-- リストモナドに対してfilterMを用いると、冪集合を極めて簡潔に計算できる
-- それぞれの要素を含める場合と含めない場合、すべての可能性を網羅すればよい
main2 =
  print $ filterM (\x -> [True,False]) [1,2,3]
    -- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

{--
> :t join
join :: Monad m => m (m a) -> m a
--}

main3 = do
  print $ join [[1,2],[3,4],[5,6]]
  print $ join (Just (Just 1))