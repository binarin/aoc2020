module Day09 (day09) where

import qualified Data.Set as S
import Data.Set (Set)
import Debug.Trace (traceShow)


type NumWithSums = (Int, Set Int)

addNum :: Int -> [NumWithSums] -> Int -> [NumWithSums]
addNum depth sums num = new : preceeding
  where
    preceeding = take (depth - 1) sums
    preceedingNums = fst <$> preceeding
    new = (num, S.fromList $ fmap (+num) preceedingNums)

isSumOfPrevious :: Int -> [NumWithSums] -> Bool
isSumOfPrevious num sums = or $ S.member num . snd <$> sums


nonSums :: Int -> [Int] -> [NumWithSums] -> [Int]
nonSums _ [] sums = []
nonSums depth (n:ns) sums
  | isSumOfPrevious n sums = cont
  | otherwise = n:cont
  where sums' = addNum depth sums n
        cont = nonSums depth ns sums'


encryptionWeakness :: Int -> [Int] -> Int
encryptionWeakness target (n:ns) = go ns ns 0 n
  where
    go start@(s:ss) end@(e:es) sum beforeEnd
      | sum > target = go ss end (sum - s) beforeEnd
      | sum < target = go start es (sum + e) e
      | sum == target = (minimum $ beforeEnd:takeWhile (/= beforeEnd) start) +
                        (maximum $ beforeEnd:takeWhile (/= beforeEnd) start)


day09 :: IO ()
day09 = do
  input :: [Int] <- fmap read . lines <$> readFile "data/09.txt"
  let depth = 25
      (preamble, message) = splitAt depth input
      preambleSums = foldl (addNum depth) [] preamble
      invalid = head $ nonSums depth message preambleSums

  -- print $ preambleSums
  -- print $ isSumOfPrevious (head message) preambleSums
  print invalid
  print $ encryptionWeakness invalid input

  pure ()
