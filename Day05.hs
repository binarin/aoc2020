module Day05 (day05) where

import Numeric (readInt)
import qualified Data.Set as S

samples = [ "FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL" ]

decodeSeat :: String -> Int
decodeSeat = fst . head . readInt 2 validDigit convertDigit
  where
    validDigit d = d `elem` "FBLR"
    convertDigit 'F' = 0
    convertDigit 'B' = 1
    convertDigit 'L' = 0
    convertDigit 'R' = 1

day05 :: IO ()
day05 = do
  print $ decodeSeat <$> samples
  input <- lines <$> readFile "data/05.txt"

  let seats = S.fromList $ decodeSeat <$> input
      mySeat = [ idx | idx <- [1..1022]
                     , S.member (idx - 1) seats
                     , S.member (idx + 1) seats
                     , not (S.member idx seats)
                     ]

  print $ maximum seats
  print mySeat
  pure ()
