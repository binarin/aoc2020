module Day06 (day06) where

import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M



day06 :: IO ()
day06 = do
  input <- splitOn [""] . lines <$> readFile "data/06.txt"
  let groupChars = mconcat <$> input
      groupSets = S.fromList <$> groupChars
      groupUnmergedCount = fmap (\c -> (c, 1)) <$> groupChars
      groupsMap = M.fromListWith (+) <$> groupUnmergedCount
      groupsMap' = zip input groupsMap
      gg = (\(people, m) -> length $ filter (== length people) $ M.elems m) <$> groupsMap'
  print groupChars
  print groupSets
  print groupsMap'
  print $ sum $ S.size <$> groupSets
  print $ sum $ gg
  pure ()
