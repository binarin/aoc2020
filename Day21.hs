module Day21 (day21) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (find, intercalate, sortBy)

inputParser :: Parsec String () [(S.Set String, S.Set String)]
inputParser = line `sepEndBy` string "\n"
  where
    line = (,) <$> ingridients <*> contains
    ingridients = S.fromList <$> many1 letter `sepEndBy1` string " "
    contains = S.fromList <$> (string "(contains " *> allergens <* string ")")
    allergens = many1 letter `sepBy1` string ", "

possiblyContains :: String -> [(S.Set String, S.Set String)] -> S.Set String
possiblyContains alrg foodList = foldr1 S.intersection ingridients
  where
    relevant = filter (\(_, as) -> S.member alrg as) foodList
    ingridients = fst <$> relevant

resolve :: [(S.Set String, String)] -> [(S.Set String, String)]
resolve ps = case find (\(is, _) -> S.size is == 1) ps of
               Nothing -> ps
               Just (ing, alrg) -> (ing, alrg):resolve (removeSelf alrg ing)
  where removeSelf alrg ing = (\(is, a) -> (S.difference is ing, a)) <$> filter (\(_, a) -> a /= alrg) ps


day21 :: IO ()
day21 = do
  input <- readFile "data/21.txt"
  case parse inputParser "(source)" input of
    Left err -> error $ show err
    Right parsed -> do
      let allAllergens = foldr1 S.union (snd <$> parsed)
          possibleMap = (\alg -> (possiblyContains alg parsed, alg)) <$> S.toList allAllergens
          resolved = resolve possibleMap
          contaminated = mconcat $ fst <$> resolved
          clean = sum $ (S.size . flip S.difference contaminated) <$> (fmap fst parsed)
          sorted = sortBy (\a b -> compare (snd a) (snd b)) resolved
      print clean
      putStrLn $ intercalate "," $ mconcat $ (S.toList . fst) <$> sorted
      pure ()
  pure ()
