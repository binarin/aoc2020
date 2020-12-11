module Day07 (day07) where

import qualified Data.Map as M
import Data.Map (Map)

import qualified Data.Set as S
import Data.Set (Set)

import Data.Functor (($>))
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Control.Monad (void)

bag :: Parsec String () ()
bag = void $ try (string "bags") <|> try (string "bag")

color :: Parsec String () String
color = many1 letter

coloredBag :: Parsec String () String
coloredBag = unwords <$> manyTill (color <* string " ") bag

bagWithCount :: Parsec String () (String, Int)
bagWithCount = do
  count <- decimal
  string " "
  color <- coloredBag
  pure (color, count)

lineParser :: Parsec String () (String, [(String, Int)])
lineParser = do
  thisBag <- coloredBag
  string " contain "
  let noBags = string "no other bags" $> []
  bags <- try noBags <|> bagWithCount `sepBy` string ", "
  string "."
  pure (thisBag, bags)

day07 :: IO ()
day07 = do
  input <- readFile "data/07.txt"
  -- print $ parse lineParser "(source)" input
  case parse (lineParser `sepEndBy` string "\n") "(source)" input of
    Left e -> error $ show e
    Right parsed -> do
      let buildContains :: (String, [(String, Int)]) -> Map String (Set String) -> Map String (Set String)
          buildContains (_, []) m = m
          buildContains (outer, (inner, _):is) m = buildContains (outer, is) m'
            where
              m' = M.insertWith S.union inner (S.singleton outer) m
          contains = foldr buildContains M.empty parsed

          walkContains :: Map String (Set String) -> [String] -> [String]
          walkContains m [] = []
          walkContains m (b:bs) = direct ++ walkContains m (direct ++ bs)
            where
              direct = case M.lookup b m of
                         Nothing -> []
                         Just s -> S.toList s

      print $ S.size $ S.fromList $ walkContains contains ["shiny gold"]
      pure ()
  pure ()
