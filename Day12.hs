module Day12 (day12) where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

data Dir = N | S | E | W deriving (Eq, Show)
data Command = Move Dir Int | L Int | R Int | F Int deriving (Eq, Show)

data Ship = Ship Dir Int Int deriving (Show)

dirParser :: Parsec String () Dir
dirParser = N <$ string "N"
  <|> E <$ string "E"
  <|> W <$ string "W"
  <|> S <$ string "S"

commandParser :: Parsec String () Command
commandParser =
  Move <$> dirParser <*> decimal
  <|> L <$> (string "L" *> decimal)
  <|> R <$> (string "R" *> decimal)
  <|> F <$> (string "F" *> decimal)

defaultState = Ship E 0 0

executeCommand :: Ship -> Command -> Ship
executeCommand (Ship d x y) (Move N dc) = Ship d x (y - dc)
executeCommand (Ship d x y) (Move S dc) = Ship d x (y + dc)
executeCommand (Ship d x y) (Move E dc) = Ship d (x + dc) y
executeCommand (Ship d x y) (Move W dc) = Ship d (x - dc) y
executeCommand (Ship d x y) (R da) = Ship (turnRight d $ da `div` 90) x y
executeCommand (Ship d x y) (L da) = Ship (turnLeft d $ da `div` 90) x y
executeCommand (Ship d x y) (F dc) = Ship d (x + dirDx d * dc) (y + dirDy d * dc)

turnLeft d 0 = d
turnLeft N c = turnLeft W (c - 1)
turnLeft W c = turnLeft S (c - 1)
turnLeft S c = turnLeft E (c - 1)
turnLeft E c = turnLeft N (c - 1)

turnRight d 0 = d
turnRight N c = turnRight E (c - 1)
turnRight E c = turnRight S (c - 1)
turnRight S c = turnRight W (c - 1)
turnRight W c = turnRight N (c - 1)

dirDx N = 0
dirDx S = 0
dirDx E = 1
dirDx W = -1

dirDy N = -1
dirDy S = 1
dirDy E = 0
dirDy W = 0

data Ship2 = Ship2 Int Int Int Int deriving (Eq, Show)

defaultShip2 = Ship2 10 (-1) 0 0

executeCommand' :: Ship2 -> Command -> Ship2
executeCommand' (Ship2 wx wy x y) (Move N dc) = Ship2 wx (wy - dc) x y
executeCommand' (Ship2 wx wy x y) (Move S dc) = Ship2 wx (wy + dc) x y
executeCommand' (Ship2 wx wy x y) (Move E dc) = Ship2 (wx + dc) wy x y
executeCommand' (Ship2 wx wy x y) (Move W dc) = Ship2 (wx - dc) wy x y
executeCommand' sh (L da) = turnLeft' sh (da `div` 90)
executeCommand' sh (R da) = turnRight' sh (da `div` 90)
executeCommand' (Ship2 wx wy x y) (F dw) = Ship2 wx wy (x + dw * wx) (y + dw * wy)

turnLeft' sh 0 = sh
turnLeft' (Ship2 wx wy x y) c = turnLeft' (Ship2 wy (- wx) x y) (c - 1)


turnRight' sh 0 = sh
turnRight' (Ship2 wx wy x y) c = turnRight' (Ship2 (- wy) wx x y) (c - 1)


day12 :: IO ()
day12 = do
  input <- readFile "data/12.txt"
  case parse (commandParser `sepEndBy` string "\n") "(source)" input of
    Left err -> error $ show err
    Right parsed -> do
      -- print $ parsed
      let Ship _ x y = foldl executeCommand defaultState parsed
      print $ abs x + abs y

      let Ship2 _ _ x' y' = foldl executeCommand' defaultShip2 parsed
      print $ abs x' + abs y'
      pure ()
  pure ()
