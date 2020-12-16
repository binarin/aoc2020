module Day16 (day16) where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

import qualified Data.Set as S

import Data.List

import Control.Lens

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
  (decimal `sepBy1` string ",") `sepEndBy` string "\n"

type Range = (Int, Int)
type FieldSpec = (String, Range, Range)
data Input = Input [FieldSpec] [Int] [[Int]] deriving (Show)

inputParser :: Parsec String () Input
inputParser =
  Input <$> (fieldSpecParser `sepEndBy` string "\n" <* string "\n")
        <*> (yourTicketParser <* string "\n")
        <*> nearbyTicketsParser

looksLikeField :: Int -> FieldSpec -> Bool
looksLikeField num (_, (s1,e1), (s2,e2))
  | num >= s1 && num <= e1 = True
  | num >= s2 && num <= e2 = True
  | otherwise = False

classifyFields :: [FieldSpec] -> [Int] -> [S.Set String]
classifyFields fs nums = [ S.fromList (view _1 <$> filter (looksLikeField num) fs) | num <- nums ]

resolveGuess :: [S.Set String] -> [String]
resolveGuess = fmap (either (const "XXX") id) . go . fmap Left
  where
    shouldResolve (Left s) = S.size s == 1
    shouldResolve _ = False
    go xs = case break shouldResolve xs of
              (_, []) -> xs
              (pre, Left this:post) -> go $ (reduceGuess this <$> pre) ++ [Right $ head $ S.toList this] ++ (reduceGuess this <$> post)
    reduceGuess g (Left it) = Left $ S.difference it g
    reduceGuess _ o = o


day16 :: IO ()
day16 = do
  input <- readFile "data/16.txt"
  case parse inputParser "(source)" input of
    Left err -> error $ show err
    Right (Input rules my nearby) -> do
      let validSet = S.fromList $ mconcat $ fmap (\(_, (s1,e1), (s2,e2)) -> [s1..e1] ++ [s2..e2]) rules
          invalidNumbers = filter (not . (`S.member` validSet)) (mconcat nearby)
          valid = filter (not . any S.null) (classifyFields rules <$> nearby)
          guess = foldl1 (zipWith S.intersection) valid
          myTicket = zip (resolveGuess guess) my

      print $ sum invalidNumbers
      print $ product $ view _2 <$> filter (("departure" `isPrefixOf`) . view _1)  myTicket
  pure ()
