module Day04 (day04) where

import Data.List.Split (splitOn)
import Data.List (partition)
import Data.Char (isDigit, isHexDigit)

data Passport = Passport { _fields :: [String]
                         , _cid :: Maybe String
                         } deriving (Show)

validateByr str = 1920 <= year && year <= 2002
  where
    year = read str

validateIyr str = 2010 <= year && year <= 2020
  where
    year = read str

validateEyr str = 2020 <= year && year <= 2030
  where
    year = read str

validateHgt str = case unit of
  "cm" -> 150 <= hgt && hgt <= 193
  "in" -> 59 <= hgt && hgt <= 79
  _ -> False
  where
    (hgtStr, unit) = span isDigit str
    hgt = read hgtStr

validateHcl ('#':digits) = 6 == length digits && all isHexDigit digits
validateHcl _ = False

validateEcl "amb" = True
validateEcl "blu" = True
validateEcl "brn" = True
validateEcl "gry" = True
validateEcl "grn" = True
validateEcl "hzl" = True
validateEcl "oth" = True
validateEcl _ = False

validatePid str = 9 == length str && all isDigit str

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

isValid' :: Passport -> Bool
isValid' (Passport flds _) = 7 == length flds && all isValidField flds
  where
    isValidField fld = case break (== ':') fld of
      ("byr", ':':val) -> validateByr val
      ("iyr", ':':val) -> validateIyr val
      ("eyr", ':':val) -> validateEyr val
      ("hgt", ':':val) -> validateHgt val
      ("hcl", ':':val) -> validateHcl val
      ("ecl", ':':val) -> validateEcl val
      ("pid", ':':val) -> validatePid val
      e -> error $ show e


day04 :: IO ()
day04 = do
  -- let validPassport = parsePassport ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"]
  -- print $ isValid' validPassport

  input <- parsePassports <$> readFile "data/04.txt"
  -- print $ input
  print $ length $ filter isValid input
  print $ length $ filter isValid' input
  pure ()
