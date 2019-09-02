module Votes where

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

-- それぞれの投票者が候補者の名前をいくつでも投票用紙に書けるような投票方法 
-- 投票用紙に書く名前には、第一の選択、第二の選択というように、順位を付けるものとします。
-- 当選者を決める方法は以下のとおりです。
-- まず、名前が一つも書かれていない投票用紙を取り除きます。
-- 次に、第一の選択として名前を書かれた票数が最低だった候補者を取り除きます。
-- この手順を候補者が一人になるまで繰り返します。残った候補者が当選者です。

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map $ filter (/= x)
-- elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c] -> c
               (c:cs) -> winner' (elim c bs)

main2 = do
  print $ map head ballots
    -- ["Red","Blue","Green","Blue","Green"]
  print $ result . map head $ ballots
    -- [(1,"Red"),(2,"Blue"),(2,"Green")]
  print $ rank ballots
    -- ["Red","Blue","Green"]
  print $ winner' ballots
    -- "Green"

