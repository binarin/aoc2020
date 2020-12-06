module Day03 (day03) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V


data Grid = Grid { _width :: Int
                 , _height :: Int
                 , _cells :: Vector Bool
                 } deriving (Show)

hasTree :: Grid -> Int -> Int -> Bool
hasTree (Grid w _ cs) x y = cs ! idx
  where
    xZero = x - 1
    yZero = y - 1
    clampedX = xZero `mod` w
    idx = yZero * w + clampedX

parseGrid :: String -> Grid
parseGrid str = Grid width height cells
  where
    rows = lines str
    width = length $ head rows
    height = length rows
    parseCell '.' = False
    parseCell '#' = True
    cells = V.fromList $ mconcat $ fmap (fmap parseCell) rows


evaluateSlope :: Grid -> Int -> Int -> Int
evaluateSlope gr@(Grid _ h _) dx dy =
  length $ filter (== True) [ hasTree gr (1 + dx * step) (1 + dy * step) | step <- [0..(h `div` dy - 1)] ]

day03 :: IO ()
day03 = do
  grid <- parseGrid <$> readFile "data/03.txt"
  print $ evaluateSlope grid 3 1
  let slopes = [(1,1), (3,1), (5,1), (7, 1), (1, 2)]
  print $ product $  uncurry (evaluateSlope grid) <$> slopes
  pure ()
