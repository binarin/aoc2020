module Day11 (day11) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.List (intercalate)
import Data.List.Split (chunksOf)

data Grid a = Grid { _width :: Int
                   , _height :: Int
                   , _cells :: Vector a
                   , _outOfBounds :: a
                   } deriving (Show, Eq)

at :: Grid a -> (Int, Int) -> a
(Grid w h cs oob) `at` (x, y)
  | x < 0 || x >= w || y < 0 || y >= h = oob
  | otherwise = cs ! (w * y + x)


parseGrid :: String -> (Char -> a) -> a -> Grid a
parseGrid str f oob = Grid width height cells oob
  where
    rows = lines str
    height = length rows
    width = length $ head rows
    cells = V.fromList $ f <$> mconcat rows

mapGrid :: ((Int, Int) -> a -> Grid a -> a) -> Grid a -> Grid a
mapGrid f gr@(Grid w h cs oob) = Grid w h cs' oob
  where
    cs' = V.imap (\idx a -> f (idx `mod` w, idx `div` w) a gr) cs

renderGrid :: (a -> String) -> Grid a -> String
renderGrid f (Grid w h cs _) = intercalate "\n" (mconcat <$> rows)
  where
    rows = chunksOf w (f <$> V.toList cs)

data Cell = Empty | Occupied | Floor deriving (Show, Eq)

parseCell '.' = Floor
parseCell 'L' = Empty
parseCell '#' = Occupied
parseCell c = error $ "Unknown char " ++ [c]

printCell Floor = "."
printCell Empty = "L"
printCell Occupied = "#"

adjacentCount f (x,y) gr = length $ filter f [ gr `at` (x-1,y-1)
                                             , gr `at` (x-1,y)
                                             , gr `at` (x-1,y+1)
                                             , gr `at` (x,y-1)
                                             , gr `at` (x,y+1)
                                             , gr `at` (x+1,y-1)
                                             , gr `at` (x+1,y)
                                             , gr `at` (x+1,y+1)
                                             ]

gridRay :: (Int, Int) -> (Int, Int) -> Grid a -> [a]
gridRay (x, y) (dx, dy) gr@(Grid w h _ _) = (gr `at`) <$> takeWhile inBounds ray
  where
    inBounds (x, y) = x >= 0 && x < w && y >= 0 && y < h
    ray = tail $ iterate (\(x,y) -> (x+dx,y+dy)) (x,y)


projectedOcuupancy (x,y) gr = length $ filter (== Occupied) $ mconcat [nw, n, ne, w, e, sw, s, se]
  where
    checkDir (dx, dy) = take 1 $ dropWhile (== Floor) $ gridRay (x, y) (dx, dy) gr
    nw = checkDir (-1,-1)
    n = checkDir (0,-1)
    ne = checkDir (1,-1)
    w = checkDir (-1, 0)
    e = checkDir (1, 0)
    sw = checkDir (-1,1)
    s = checkDir (0,1)
    se = checkDir (1,1)

changeSeatState :: (Int, Int) -> Cell -> Grid Cell -> Cell
changeSeatState _ Floor _ = Floor
changeSeatState pos Empty gr = if adjacentCount (== Occupied) pos gr == 0 then Occupied else Empty
changeSeatState pos Occupied gr = if adjacentCount (== Occupied) pos gr >= 4 then Empty else Occupied

changeSeatState' :: (Int, Int) -> Cell -> Grid Cell -> Cell
changeSeatState' _ Floor _ = Floor
changeSeatState' pos Empty gr = if projectedOcuupancy pos gr == 0 then Occupied else Empty
changeSeatState' pos Occupied gr = if projectedOcuupancy pos gr >= 5 then Empty else Occupied



findDupe :: Eq a => [a] -> Maybe a
findDupe (a:b:as)
  | a == b = Just a
  | otherwise = findDupe (b:as)
findDupe _ = Nothing


day11 :: IO ()
day11 = do
  input <- readFile "data/11.txt"
  let grid = parseGrid input parseCell Floor
      seq = iterate (mapGrid changeSeatState') grid
      dupe = findDupe seq

  -- putStrLn $ renderGrid printCell grid
  -- putStrLn $ renderGrid printCell $ mapGrid changeSeatState $ mapGrid changeSeatState grid

  case dupe of
    Just it -> do
      -- putStrLn $ renderGrid printCell it
      print $ length $ filter (== Occupied) (V.toList (_cells it))
      print $ gridRay (3,5) (-1, -1) grid
  pure ()
