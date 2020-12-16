module Day16 (day16) where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

import Data.Set (Set)
import qualified Data.Set as S

rangeParser :: Parsec String () (Int, Int)
rangeParser = (,) <$> (decimal <* string "-") <*> decimal

fieldSpecParser :: Parsec String () (String, (Int, Int), (Int, Int))
fieldSpecParser = do
  name :: String <- (:) <$> letter <*> many (letter <|> space)
  string ": "
  r1 <- rangeParser
  string " or "
  r2 <- rangeParser
  pure (name, r1, r2)

yourTicketParser :: Parsec String () [Int]
yourTicketParser = do
  string "your ticket:\n"
  lst <- decimal `sepBy` string ","
  string "\n"
  pure lst


nearbyTicketsParser :: Parsec String () [[Int]]
nearbyTicketsParser = do
  string "nearby tickets:\n"
  (decimal `sepBy` string ",") `sepEndBy` string "\n"

type Range = (Int, Int)
type FieldSpec = (String, Range, Range)
data Input = Input [FieldSpec] [Int] [[Int]] deriving (Show)

inputParser :: Parsec String () Input
inputParser =
  Input <$> (fieldSpecParser `sepEndBy` string "\n" <* string "\n")
        <*> (yourTicketParser <* string "\n")
        <*> nearbyTicketsParser


day16 :: IO ()
day16 = do
  input <- readFile "data/16.txt"
  case parse inputParser "(source)" input of
    Left err -> error $ show err
    Right (Input rules my nearby) -> do
      let validSet = S.fromList $ mconcat $ fmap (\(_, (s1,e1), (s2,e2)) -> [s1..e1] ++ [s2..e2]) rules
          invalid = filter (not . (`S.member` validSet)) (mconcat nearby)
      print $ sum invalid
      pure ()
  pure ()
