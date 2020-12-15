module Day15 (day15) where

import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

-- go 10 4 6 0 (0 3 6)
-- go 10 5 0 3 (0 3 6 0)
-- go 10 6 3 3 (0 3 6 0 3)
-- go 10 7 3 1 (0 3 6 0 3 3)

go till turn prev prevDiff seen
  | till < turn = []
  | otherwise = prevDiff:(go till (turn + 1) prevDiff diff (M.insert prevDiff turn seen))
  where
    diff = case M.lookup prevDiff seen of
      Nothing -> 0
      Just oldTurn -> turn - oldTurn




day15 :: IO ()
day15 = do
  let input = [0,6,1,7,2,19,20]
  let initial = M.fromList (zip input [1..])
      calc' till ns = last $ go till (length ns + 1) (last ns) 0 (M.fromList (zip ns [1..]))
      calc = calc' 2020
  print $ calc [0,3,6]
  print $ calc [1,3,2]
  print $ calc [2,1,3]
  print $ calc [1,2,3]
  print $ calc [2,3,1]
  print $ calc [3,2,1]
  print $ calc [3,1,2]
  print $ calc [0,6,1,7,2,19,20]
  print $ calc' 30000000 [0,6,1,7,2,19,20]
  pure ()
