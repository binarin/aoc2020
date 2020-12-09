module Day09 (day09) where

import qualified Data.Set as S
import Data.Set (Set)


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



day09 :: IO ()
day09 = do
  input :: [Int] <- fmap read . lines <$> readFile "data/09.txt"
  let depth = 25
      (preamble, message) = splitAt depth input
      preambleSums = foldl (addNum depth) [] preamble

  -- print $ preambleSums
  -- print $ isSumOfPrevious (head message) preambleSums
  print $ nonSums depth message preambleSums

  pure ()
