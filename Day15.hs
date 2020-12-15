module Day15 (day15) where

import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

go :: Int -> Int -> Int -> Int -> Map Int Int -> Int
go till turn last lastDiff seen
  | turn > till = last
  | otherwise = go till (turn + 1) last' diff (M.insert last' turn seen)
    where
      last' = lastDiff
      diff = case M.lookup last' seen of
        Nothing -> 0
        Just oldTurn -> turn - oldTurn


day15 :: IO ()
day15 = do
  -- let input = [0,6,1,7,2,19,20]
  let input = [0,3,6]
  let initial = M.fromList (zip (init input) [1..])

  print ((length input),(last input), initial)
  print $ [go x (length input + 1) (last input) 0 initial | x <- [4..2021]]
  pure ()
