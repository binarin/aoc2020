module Day18 (day18) where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

termParser :: Parsec String () Int
termParser = decimal <|> (string "(" *> exprParser <* string ")")

opParser :: Parsec String () (Int -> Int -> Int)
opParser = try ((*) <$ string " * ") <|> ((+) <$ string " + ")

opWithArgParser :: Parsec String () (Int -> Int)
opWithArgParser = do
  op <- opParser
  term <- termParser
  pure (`op` term)

exprParser :: Parsec String () Int
exprParser = do
  first <- termParser
  fs <- many opWithArgParser
  pure $ foldl (\n f -> f n) first fs

parens p = string "(" *> p <* string ")"
expr    = term   `chainl1` mulop
term    = factor `chainl1` addop
factor  = parens expr <|> decimal
mulop   =   do{ try $ string " * "; return (*)   }
addop   =   do{ try $ string " + "; return (+) }



day18 :: IO ()
day18 = do
  input <- readFile "data/18.txt"
  print $ sum <$> parse (exprParser `sepEndBy` string "\n") "(source)" input
  print $ sum <$> parse (expr `sepEndBy` string "\n") "(source)" input
  pure ()
