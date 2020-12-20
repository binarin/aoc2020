module Day20 (day20) where

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad (forM_)
import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.List (elemIndex, find, findIndex, transpose)


data Tile = Tile { _id :: Int
                 , _borders :: [String]  -- N(W->E), E(N->S), S(E->W), W(S->N)
                 , _cells :: [String]
                 } deriving (Show, Eq)

parseTile :: Parsec String () Tile
parseTile = do
  tileId <- string "Tile " *> decimal <* string ":\n"
  rows <- (many1 $ oneOf ".#") `sepEndBy` string "\n"
  let borders = getBorders rows
      getBorders rows = [top, right, bottom, left]
        where
          top = head rows
          right = last <$> rows
          bottom = reverse $ last rows
          left = reverse $ head <$> rows
      stripBorders = tail . init
  pure $ Tile tileId borders (stripBorders $ stripBorders <$> rows)

a `revEq` b = a == b || reverse a == b

type Border = String
data Dir = N | E | S | W deriving (Eq, Show, Enum)

rotateRight :: Tile -> Tile
rotateRight (Tile i bs rs) = Tile i (last bs:init bs) (reverse <$> transpose rs)

looseAlignTile :: Dir -> Border -> Tile -> Tile
looseAlignTile dir brd tl@(Tile _ bs _) = tl'
  where
    Just brdPos = findIndex (\b -> b == brd || reverse b == brd) bs
    tl' = if toEnum brdPos == dir
      then tl
      else let rots = (4 + fromEnum dir - brdPos) `mod` 4
           in iterate rotateRight tl !! rots


brdAt :: Dir -> Tile -> Border
brdAt N (Tile _ [b, _, _, _] _) = b
brdAt E (Tile _ [_, b, _, _] _) = b
brdAt S (Tile _ [_, _, b, _] _) = b
brdAt W (Tile _ [_, _, _, b] _) = b

opposite :: Dir -> Dir
opposite N = S
opposite S = N
opposite E = W
opposite W = E

buildSpine :: Dir -> [Tile] -> ([Tile], [Tile])
buildSpine _ [] = ([], [])
buildSpine dir (t:ts) = (init (reverse rev) ++ [t] ++ drop 1 direct, rest')
  where
    go dir ts spine@(s:_) = case getMatching ts (brdAt dir s) of
                          Nothing -> (reverse spine, ts)
                          Just (tl, rest) -> go dir rest (looseAlignTile (opposite dir) (brdAt dir s) tl:spine)
    (direct, rest) = go dir ts [t]
    (rev, rest') = go (opposite dir) rest [t]
    getMatching ts brd = case find (\(Tile _ bs _) -> (brd `elem` bs) || (reverse brd `elem` bs)) ts of
      Nothing -> Nothing
      Just el -> Just (el, filter (/= el) ts)


flipRows :: [[Tile]] -> [[Tile]]
flipRows (r:[]) = [r]
flipRows (r1@(a:_):r2@(b:_):rest) = r1:flipRows (r2':rest)
  where
    r2' = if an `revEq` bn || an `revEq` bs || as `revEq` bn || as `revEq` bs then r2 else reverse (flipH <$> r2)
    an = brdAt N a
    as = brdAt S a
    bn = brdAt N b
    bs = brdAt S b

flipH :: Tile -> Tile
flipH (Tile i [n, e, s, w] cells) = Tile i [n, w, s, e] (reverse <$> cells)

flipV :: Tile -> Tile
flipV (Tile i [n, e, s, w] cells) = Tile i [s, e, n, w] (reverse cells)

flipCells :: [Tile] -> [Tile] -> ([Tile], [Tile])
flipCells top bot = (fst <$> pairs, snd <$> pairs)
  where
    pairs = zipWith stack top bot

stack t b
  | brdAt S t `revEq` brdAt N b = (t, b)
  | brdAt S t `revEq` brdAt S b = (t, flipV b)
  | brdAt N t `revEq` brdAt N b = (flipV t, b)
  | brdAt N t `revEq` brdAt S b = (flipV t, flipV b)
  | otherwise = error $ show (t, b, brdAt N t, brdAt S t, brdAt N b, brdAt S b)

align :: [[Tile]] -> [[Tile]]
align ([x]) = [x]
align (a:b:xs) = a':align (b':xs)
  where
    (a', b') = flipCells a b

flattenRow :: [Tile] -> [String]
flattenRow ts = foldr1 mergeCells grids
  where
    mergeCells :: [String] -> [String] -> [String]
    mergeCells l r = zipWith (++) l r
    grids :: [[String]]
    grids = _cells <$> ts

day20 :: IO ()
day20 = do
  input <- readFile "data/20.txt"
  case parse (parseTile `sepEndBy` string "\n") "(source)" input of
    Left err -> error $ show err
    Right parsed -> do
      let borders = foldr (\el m -> M.insertWith (+) (reverse el) 1 $ M.insertWith (+) el 1 m) M.empty (mconcat $ _borders <$> parsed)
          isCornerTile (Tile _ bs _) = 2 == (length $ filter (== 1) (fromJust . flip M.lookup borders <$> bs))
          corners = filter isCornerTile parsed
          (vertSpine, rest) = buildSpine S parsed
          (looseFit, []) = foldl (\(ln, rst) tl -> let (sp, rst') = buildSpine E (tl:rst) in (sp:ln, rst') ) ([], rest) vertSpine
          tilesInPlace =  flipRows looseFit
          image = mconcat $ flattenRow <$> align tilesInPlace
          width = length $ head image
          height = length image
          rotateImage im = reverse <$> transpose im
          allImages = fmap imageAsSet $ take 4 (iterate rotateImage image) ++ take 4 (iterate rotateImage $ reverse image)
          imageAsSet img = S.fromList [ (x, y) | (y, row) <- zip [0..] img, (x, c) <- zip [0..] row, c == '#' ]
          isThereDragon im x y = and [ S.member (x+dx,y+dy) im | (dx, dy) <- dragonDeltas dragon ]
          countDragons im = length $ [ () | y <- [0..height - 1], x <- [0..width - 1], isThereDragon im x y ]
          imgWithDragons = head $ filter ((> 0) . countDragons) allImages
          -- (imgWithDragons, dragons) <- head $
      -- print $ fmap (flip M.lookup borders) . _borders <$> corners
      print $ product $ _id <$> filter (flip elem corners) parsed
      -- forM_  looseFit print
      -- putStrLn "========"
      --forM_ (align tilesInPlace) (print . fmap _id)
      -- forM_ image putStrLn
      print (width, height)
      print $ S.size imgWithDragons - (countDragons imgWithDragons) * length (dragonDeltas dragon)

dragon = ["                  # "
         ,"#    ##    ##    ###"
         ," #  #  #  #  #  #   "
         ]

dragonDeltas dr = [ (x,y) | (y, row) <- zip [0..] dr, (x, cell) <- zip [0..] row, cell == '#' ]
