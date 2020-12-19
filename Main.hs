module Main where

import qualified Data.Set as S
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19

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
  day19
