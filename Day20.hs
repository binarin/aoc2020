module Day20 (day20) where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as M


data Tile = Tile { _id :: Int
                 , _borders :: [String]  -- N(W->E), E(N->S), S(E->W), W(S->N)
                 } deriving (Show)

parseTile :: Parsec String () Tile
parseTile = Tile <$> tileId <*> borders
  where
    tileId = string "Tile " *> decimal <* string ":\n"
    borders = getBorders <$> (many1 $ oneOf ".#") `sepEndBy` string "\n"
    getBorders rows = [top, right, bottom, left]
      where
        top = head rows
        right = last <$> rows
        bottom = reverse $ last rows
        left = reverse $ head <$> rows




day20 :: IO ()
day20 = do
  input <- readFile "data/20-sample.txt"
  case parse (parseTile `sepEndBy` string "\n") "(source)" input of
    Left err -> error $ show err
    Right parsed -> do
      let borders = foldr (\el m -> M.insertWith (+) (reverse el) 1 $ M.insertWith (+) el 1 m) M.empty (mconcat $ _borders <$> parsed)
          isCornerTile (Tile _ bs) = 2 == (length $ filter (== 1) (fromJust . flip M.lookup borders <$> bs))
          corners = filter isCornerTile parsed
      print $ fmap (flip M.lookup borders) . _borders <$> corners
      -- print $ filter (flip elem corners . _id) parsed
      pure ()
  pure ()
