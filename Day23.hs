module Day23 (day23) where

import Prelude hiding ((!))
import Data.Map (Map, (!))
import qualified Data.Map as M

buildRing :: [Int] -> Map Int Int
buildRing nums@(first:_) = M.fromList $ go nums
  where
    go (n1:n2:ns) = (n1, n2):go (n2:ns)
    go (n1:[]) = [(n1, first)]


rotate' :: (Int, Map Int Int) -> (Int, Map Int Int)
rotate' (cur, m) = (cur', m')
  where
    t1 = m ! cur
    t2 = m ! t1
    t3 = m ! t2
    cur' = m ! t3
    dest = head $ filter (\e -> e /= t1 && e /= t2 && e /= t3) $ tail (iterate prev' cur)
    destNext = m ! dest
    m' = M.insert cur cur' $
         M.insert dest t1 $
         M.insert t3 destNext $
         m

prev n
  | n == 1 = 9
  | otherwise = n - 1

prev' n
  | n == 1 = 1000000
  | otherwise = n - 1

-- rotate :: [Int] -> [Int]
rotate (current:rest) = mconcat $ fmap replace rest' ++ [[current]]
  where
    three@[t1, t2, t3] = take 3 rest
    rest' = drop 3 rest
    dest = head $ filter (\e -> e /= t1 && e /= t2 && e /= t3) $ tail (iterate prev current)
    replace e
      | e == dest = e:three
      | otherwise = [e]

day23 :: IO ()
day23 = do
  -- let input :: [Int] = fmap (\c -> read [c]) "389125467"
  let input :: [Int] = fmap (\c -> read [c]) "193467258"
  print $ last $ take 101 $ iterate rotate input
  let (_, ring) = head $ drop 10000000 $ iterate rotate' (head input, buildRing $ input ++ [10..1000000])
  print $ (ring ! 1, ring ! (ring ! 1))
  -- print input
  -- print $ buildRing input
  pure ()
