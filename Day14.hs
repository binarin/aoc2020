module Day14 (day14) where

import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (Word)
import Text.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number
import Data.Int (Int64)
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as M
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Control.Monad (forM_)

-- and mask, or mask
type Mask = (Word, Word)

type Word = Int64

expandMask :: String -> Mask
expandMask str = go str 0 0
  where
    go ('1':xs) orM andM = go xs (1 + 2 * orM) (1 + 2 * andM)
    go ('0':xs) orM andM = go xs (2 * orM) (2 * andM)
    go ('X':xs) orM andM = go xs (2 * orM) (1 + 2 * andM)
    go [] orM andM = (andM, orM)

maskValue :: Word -> String -> Word
maskValue val mask = (val .|. orM) .&. andM
  where (andM, orM) = expandMask mask

maskParser :: Parsec String () String
maskParser = do
  try $ string "mask = "
  many (oneOf "01X")

type Assign = (Word, Word)
assignParser :: Parsec String () Assign
assignParser = do
  string "mem["
  addr <- decimal
  string "] = "
  value <- decimal
  pure (addr, value)

toBinary :: Word -> String
toBinary n = replicate fill '0' ++ digits
  where
    digits = showIntAtBase 2 intToDigit n ""
    fill = 36 - length digits

data Command = Mask String | Assign Assign deriving (Show)

commandParser :: Parsec String () Command
commandParser = try (Mask <$> maskParser) <|> (Assign <$> assignParser)

data State = State (Map Word Word) String deriving (Show)

executeCommand (State mem _) (Mask mask') = State mem mask'
executeCommand (State mem mask) (Assign (addr, val)) = State (M.insert addr (maskValue val mask) mem) mask

data State' = State' [(Word, Set Word)] String deriving (Show)

applyAddrMask :: String -> String -> String
applyAddrMask [] _ = []
applyAddrMask _ [] = []
applyAddrMask (a:as) ('0':ms) = a:applyAddrMask as ms
applyAddrMask (_:as) ('1':ms) = '1':applyAddrMask as ms
applyAddrMask (_:as) ('X':ms) = 'X':applyAddrMask as ms

expandAddr :: String -> [Word]
expandAddr = go []
  where
    go :: String -> String -> [Word]
    go prefix [] = [read (reverse prefix)]
    go prefix ('X':as) = go ('0':prefix) as ++ go ('1':prefix) as
    go prefix (a:as) = go (a:prefix) as

executeCommand' (State' prev _) (Mask mask') = State' prev mask'
executeCommand' (State' prev mask) (Assign (addr, val)) = State' ((val, valAddrs):reducedPrev) mask
  where
    valAddrs = S.fromList $ expandAddr $ applyAddrMask (toBinary addr) mask
    reducedPrev = fmap (`S.difference` valAddrs) <$> prev


day14 :: IO ()
day14 = do
  input <- readFile "data/14.txt"
  let parsed = parse (commandParser `sepEndBy` string "\n") "(source)" input
      freshState = State M.empty ""
  case parsed of
    Left err -> error $ show err
    Right commands -> do
      let State mem _ = foldl executeCommand freshState commands
      print $ sum $ M.elems mem
      -- print $ expandAddr $ applyAddrMask (toBinary 42) "000000000000000000000000000000X1001X"
      -- print $ expandAddr $ applyAddrMask (toBinary 26) "00000000000000000000000000000000X0XX"
      -- putStrLn "===================="
      -- forM_  (scanl executeCommand' (State' [] "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX") commands) $ \(State' addrs _) -> do
      --   print addrs

      let (State' addrs _) = foldl executeCommand' (State' [] "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX") commands
          memSum = sum $ fmap (\(val :: Word, addrs) -> val * fromIntegral (S.size addrs)) addrs
      print memSum
      -- forM_ addrs $ \(val, mask) -> do
      --   print $ addrPower mask
      pure ()


  -- print $ maskValue 11 (expandMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
  -- print $ maskValue 101 (expandMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
  -- print $ maskValue 0 (expandMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

  pure ()
