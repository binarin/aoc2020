module Day02 (day02) where

import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec.Number

data Input = Input { _inputMinOccurence :: Int
                   , _inputMaxOccurence :: Int
                   , _inputMustOccur :: Char
                   , _inputPassword :: String
                   } deriving (Show)

parse rule = Parsec.parse rule "(source)"

inputParser = Input
  <$> decimal <* Parsec.string "-"
  <*> decimal <* Parsec.string " "
  <*> Parsec.anyChar <* Parsec.string ": "
  <*> Parsec.many (Parsec.noneOf "\n")


isValid :: Input -> Bool
isValid (Input min max char pass) = min <= cnt && cnt <= max
  where
    cnt = length $ filter (== char) pass

isValid' :: Input -> Bool
isValid' (Input p1 p2 char pass) = 1 == length (filter (== char) [pass !! (p1-1), pass !! (p2-1)])


day02 :: IO ()
day02 = do
  parsed <- parse (Parsec.many $ inputParser <* Parsec.newline) <$> readFile "data/02.txt"
  let answer = either (error . show) (length . filter isValid) parsed
  print answer
  let answer' = either (error . show) (length . filter isValid') parsed
  print answer'
  print [isValid' (Input 1 3 'a' "abcde")]
