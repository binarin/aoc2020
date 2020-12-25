module Day25 (day25) where

import Data.List
import Math.NumberTheory.Powers.Modular
import Data.Maybe

modulo :: Int
modulo = 20201227

powers a = 1:iterate (\c -> (c * a) `mod` modulo) a

pubToPriv x = fromJust $ elemIndex x (powers 7)

day25 :: IO ()
day25 = do
  -- let (cardPub, doorPub) = (5764801, 17807724)
  let (cardPub, doorPub) = (17115212, 3667832)
  let doorSec = pubToPriv doorPub
      cardSec = pubToPriv cardPub
  print (cardSec, doorSec)
  print $ powModInt doorPub cardSec modulo
  pure ()
