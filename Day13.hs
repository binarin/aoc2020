module Day13 (day13) where

import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

import Math.NumberTheory.Euclidean (extendedGCD)

-- https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
combine (adv1, p1) (adv2, p2) = (-combinedPhase, combinedPeriod)
  where
    ph1 = -adv1
    ph2 = -adv2
    phaseDiff = ph1 - ph2
    (gcd, s, t) = extendedGCD p1 p2
    (pd_mult, pd_remainder) = phaseDiff `divMod` gcd
    combinedPeriod = (p1 `div` gcd) * p2
    combinedPhase = (ph1 - s * pd_mult * p1) `mod` combinedPeriod

day13 :: IO ()
day13 = do
  [tsRaw, busesRawStr] <- lines <$> readFile "data/13.txt"
  let ts :: Integer = read tsRaw
      buses :: [Maybe Integer] = readMaybe <$> splitOn "," busesRawStr
      knownBuses = fromJust <$> filter (/= Nothing) buses
      delay b = (b * (trips + 1) - ts, b)
        where trips = ts `div` b
      (minDelta, minBus) = minimum $ delay <$> knownBuses
      periods = fmap (\(i,b) -> (i, fromJust b)) $ filter (\(_, b) -> b /= Nothing) $ zip [0..] buses
      combined = foldr1 combine periods
  print $ minDelta * minBus
  print $ combined
  print $ fst combined + snd combined
  pure ()
