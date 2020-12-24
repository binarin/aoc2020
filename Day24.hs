module Day24 (day24) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Parsec
import Text.Parsec.Char

data Dir = E | SE | SW | W | NW | NE deriving (Eq, Show)
type Coord = (Int, Int)
type Grid = S.Set Coord

flipTile :: Grid -> Coord -> Grid
flipTile g c
  | S.member c g = S.delete c g
  | otherwise = S.insert c g

move :: Dir -> Coord -> Coord
move E (x, y) = (x + 2, y)
move W (x, y) = (x - 2, y)
move SE (x, y) = (x + 1, y + 1)
move SW (x, y) = (x - 1, y + 1)
move NW (x, y) = (x - 1, y - 1)
move NE (x, y) = (x + 1, y - 1)

dirParser :: Parsec String () Dir
dirParser = (SE <$ try (string "se"))
        <|> (SW <$ try (string "sw"))
        <|> (NW <$ try (string "nw"))
        <|> (NE <$ try (string "ne"))
        <|> (E  <$ try (string "e" ))
        <|> (W  <$ try (string "w" ))

identify :: [Dir] -> Coord
identify dirs = go dirs (0, 0)
  where
    go [] c = c
    go (d:ds) c = go ds (move d c)

bounds :: Grid -> (Coord, Coord)
bounds g = (minC, maxC)
  where
    coords = S.toList g
    xs = fmap fst coords
    ys = fmap snd coords
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    minOff = if minY `mod` 2 == 0 then 2 else 3
    maxOff = if maxY `mod` 2 == 0 then 2 else 3
    minC = (iterate (move NW) (minX - (minX+minY) `mod` 2,minY)) !! minOff
    maxC = (iterate (move SE) (maxX - (maxX+maxY) `mod` 2,maxY)) !! maxOff

neighbours :: Coord -> Grid -> Int
neighbours c gr = length hasNeigbour
  where
    nCoords = flip move c <$> [W,E,NW,NE,SW,SE]
    hasNeigbour = filter (flip S.member gr) nCoords

allCoords gr = [ (x, y) | y <- [minY..maxY], x <- [minX-(y `mod` 2),minX-(y `mod` 2)+2..maxX] ]
  where
    ((minX,minY), (maxX, maxY)) = bounds gr

simulate :: Grid -> Grid
simulate gr = S.fromList newBlack
  where
    shouldBeBlack c
      | S.member c gr = ns == 1 || ns == 2
      | otherwise = ns == 2
      where
        ns = neighbours c gr
    newBlack = filter shouldBeBlack (allCoords gr)


day24 :: IO ()
day24 = do
  input <- readFile "data/24.txt"
  case parse (many1 dirParser `sepEndBy` string "\n") "(source)" input of
    Left err -> error $ show err
    Right parsed -> do
      let initial = foldl flipTile S.empty $ fmap identify parsed
      print $ S.size initial
      print $ S.size $ (iterate simulate initial) !! 100
      pure ()
  pure ()
