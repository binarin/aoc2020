module Main where

import qualified Data.Set as S
import Day02
import Day03
import Day04
import Day08

day01 = do
  nums :: [Int] <- fmap read . lines <$> readFile "data/01.txt"
  let lookup = S.fromList nums
      solution = filter (flip S.member lookup . (2020-)) nums
  print $ head solution * (solution !! 1)

  let solution2 = [ n1 * n2 * (2020 - n1 - n2)
                  | n1 <- nums
                  , n2 <- nums
                  , S.member (2020 - n1 - n2) lookup
                  ]

  print solution2

main :: IO ()
main = do
  day08
