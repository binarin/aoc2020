module Day13 (day13) where

import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

day13 :: IO ()
day13 = do
  [tsRaw, busesRawStr] <- lines <$> readFile "data/13.txt"
  let ts :: Int = read tsRaw
      buses :: [Maybe Int] = readMaybe <$> splitOn "," busesRawStr
      knownBuses = fromJust <$> filter (/= Nothing) buses
      delay b = (b * (trips + 1) - ts, b)
        where trips = ts `div` b
      (minDelta, minBus) = minimum $ delay <$> knownBuses
  print $ minDelta * minBus
  pure ()
