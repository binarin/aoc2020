module Day17 (day17) where

import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (forM_)

import Control.Lens

type Coord = (Int, Int, Int, Int)

parseGrid :: String -> Set Coord
parseGrid s = S.fromList [(x, y, 0, 0) | (y, row) <- zip [0..] (lines s)
                                    , (x, cell) <- zip [0..] row
                                    , cell == '#'
                                    ]

bounds :: Set Coord -> (Coord, Coord)
bounds grid = ((minimum xs, minimum ys, minimum zs, minimum vs), (maximum xs, maximum ys, maximum zs, maximum vs))
  where
    coords = S.toList grid
    xs = view _1 <$> coords
    ys = view _2 <$> coords
    zs = view _3 <$> coords
    vs = view _4 <$> coords

neighbourCount :: Set Coord -> Coord -> Int
neighbourCount gr (x, y, z, v) = length $ filter (`S.member` gr) coords
  where
    coords = [ (x + dx, y + dy, z + dz, v + dv) | dx <- [-1..1]
                                        , dy <- [-1..1]
                                        , dz <- [-1..1]
                                        , dv <- [-1..1]
                                        , dx /=0 || dy /= 0 || dz /= 0 || dv /= 0
                                        ]

simulateCycle :: Set Coord -> Set Coord
simulateCycle gr = S.fromList [ (x, y, z, v)
                              | x <- [(minX-1)..(maxX+1)]
                              , y <- [(minY-1)..(maxY+1)]
                              , z <- [(minZ-1)..(maxZ+1)]
                              , v <- [(minV-1)..(maxV+1)]
                              , let nc = neighbourCount gr (x, y, z, v)
                              , isActive (S.member (x,y,z,v) gr) nc
                              ]
  where
    ((minX, minY, minZ, minV), (maxX, maxY, maxZ, maxV)) = bounds gr
    isActive True nc
      | nc == 2 || nc == 3 = True
      | otherwise = False
    isActive False nc
      | nc == 3 = True
      | otherwise = False


-- printGrid :: Set Coord -> IO ()
-- printGrid gr =
--   forM_ [minZ..maxZ] $ \z -> do
--     putStrLn $ "Z=" ++ show z ++ " x=" ++ show (minX, maxX) ++ ", y=" ++ show (minY, maxY)
--     forM_ [minY..maxY] $ \y -> do
--       forM_ [minX..maxX] $ \x -> do
--         putStr $ if S.member (x, y, z) gr then "#" else "."
--       putStr "\n"
--     putStr "\n"

--   where
--     ((minX, minY, minZ), (maxX, maxY, maxZ)) = bounds gr



day17 :: IO ()
day17 = do
  input <- readFile "data/17.txt"
  let parsed = parseGrid input
      simulation = iterate simulateCycle parsed
  -- print $ neighbourCount parsed (
  -- printGrid $ simulation !! 0
  -- print $ neighbourCount parsed (0, 0, -1)
  -- print parsed
  -- print $ bounds parsed
  print $ fmap S.size $ take 7 simulation
  pure ()
