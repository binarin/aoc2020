module Day10 (day10) where

import Data.List (sort)
import Data.List.Split


calcDiff :: [Int] -> [Int]
calcDiff (cur:next:jolts) = (next-cur):calcDiff (next:jolts)
calcDiff _ = []


day10 :: IO ()
day10 = do
  input :: [Int] <- fmap read . lines <$> readFile "data/10.txt"
  let sorted = sort input
      fullSeq = (0:sorted) ++ [maximum sorted + 3]
      diffs = calcDiff fullSeq
      ones = length $ filter (== 1) diffs
      threes = length $ filter (== 3) diffs
      opportunities (1:1:1:[]) = 7
      opportunities (1:1:[]) = 4
      opportunities (1:[]) = 2
      opportunities [] = 1


  print fullSeq
  print diffs
  print $ ones * threes
  print $ foldr (*) 1 (opportunities <$> (filter (/= 3) <$> splitOn [1,3] diffs))
  pure ()

-- 2 x 2 x 2 x 7 x 7 x 7 x 7
a = [1,1,1
    ,1,3
    ,1,1,1
    ,1,3,3
    ,1
    ,1
    ,1,3
    ,1
    ,1,3,3
    ,1,1,1
    ,1,3
    ,1,3,3
    ,1,1,1
    ,1,3
    ]

b = [1,3
    ,1
    ,1
    ,1,3
    ,1
    ,1,3
    ,1,3,3
    ]
