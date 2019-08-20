-- 7.7 投票アルゴリズム

import Data.List


-- ----------------------------------------------
-- 7.7.1 比較多数得票

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- ポイントフリーにしない場合は
count' :: Eq a => a -> [a] -> Int
count' x xs = length (filter (== x) xs)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

main1 = do
  print $ count "Red" votes
    -- 2
  print $ rmdups votes
    -- ["Red","Blue","Green"]
  print $ result votes
    -- [(1,"Green"),(2,"Red"),(3,"Blue")]
  print $ winner votes
    -- "Blue"

-- ----------------------------------------------
-- 7.7.2 別の投票アルゴリズム

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]




