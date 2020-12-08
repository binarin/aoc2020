module Day08 where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
    deriving (Show)

type Program = Vector Instruction
data Machine = Machine { _program :: Program
                       , _ip :: Int
                       , _acc :: Int
                       , _instructionVisited :: Set Int
                       } deriving (Show)

data ExitCode = Loop Int | NormalExit Int deriving (Show)

instructionParser =
  Nop <$> (string "nop " >> int)
  <|> Acc <$> (string "acc " >> int)
  <|> Jmp <$> (string "jmp " >> int)

parseProgram :: String -> Either String Program
parseProgram str = bimap show V.fromList (parse (sepEndBy instructionParser (string "\n")) "(source)" str)

runMachine :: Machine -> Either ExitCode Machine
runMachine (Machine prog ip acc seen)
  | S.member ip seen = Left $ Loop acc
  | ip == V.length prog = Left $ NormalExit acc
  | otherwise = case prog ! ip of
         Nop _ -> Right $ Machine prog (ip + 1) acc seen'
         Acc d -> Right $ Machine prog (ip + 1) (acc + d) seen'
         Jmp d -> Right $ Machine prog (ip + d) acc seen'
  where
    seen' = S.insert ip seen


findLoop :: Machine -> ExitCode
findLoop mach = case runMachine mach of
                  Left acc -> acc
                  Right mach' -> findLoop mach'

flipPrograms :: Program -> [Program]
flipPrograms prg = [ flipInstruction idx | idx <- [0..V.length prg - 1] ]
  where
    flipInstruction idx = prg // [(idx, case prg ! idx of
      Nop d -> Jmp d
      Jmp d -> Nop d
      other -> other)]

day08 :: IO ()
day08 = do
  program <- readFile "data/08.txt"
  case parseProgram program of
    Left err -> error err
    Right prog -> do
      let mach = Machine prog 0 0 S.empty
      print $ findLoop mach

      let allPrograms = flipPrograms prog
      print $ filter isNormalExit $ findLoop . (\prog -> Machine prog 0 0 S.empty) <$> allPrograms
      pure ()
  pure ()
  where
    isNormalExit (NormalExit _) = True
    isNormalExit _ = False
