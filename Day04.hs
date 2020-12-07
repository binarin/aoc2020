module Day04 (day04) where

import Data.List.Split (splitOn)
import Data.List (partition)

data Passport = Passport { _fields :: [String]
                         , _cid :: Maybe String
                         } deriving (Show)


mkPassport :: [String] -> Passport
mkPassport flds = Passport mandatory cid
  where
    (maybeCidAsList, mandatory) = partition isCid flds
    cid = case maybeCidAsList of
      [] -> Nothing
      cid:_ -> Just cid
    isCid ('c':'i':'d':':':_) = True
    isCid _ = False

parsePassport :: [String] -> Passport
parsePassport rows = go rows []
  where
    go [] flds = mkPassport flds
    go (r:rs) flds = go rs (splitOn " " r ++ flds)

parsePassports :: String -> [Passport]
parsePassports str = parsePassport <$> splitOn [""] (lines str)

isValid :: Passport -> Bool
isValid (Passport flds _) = 7 == length flds

day04 :: IO ()
day04 = do
  input <- parsePassports <$> readFile "data/04.txt"
  print $ length $ filter isValid input
  pure ()
