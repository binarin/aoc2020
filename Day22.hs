module Day22 (day22) where


import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Bifunctor

playRound :: ([Int], [Int]) -> ([Int], [Int])
playRound ((p1:p1s),  (p2:p2s))
  | p1 < p2 = (p1s, p2s ++ [p2, p1])
  | otherwise = (p1s ++ [p1, p2], p2s)

playRec :: [Int] -> [Int] -> S.Set ([Int], [Int]) -> Either [Int] [Int]
playRec [] p2 _ = Right p2
playRec p1 [] _ = Left p1
playRec p1@(p1c:p1cs) p2@(p2c:p2cs) seen
  | S.member (p1, p2) seen = Left p1
  | length recp1 == p1c && length recp2 == p2c =
      case playRec recp1 recp2 S.empty of
        Left _ -> leftRound
        Right _ -> rightRound
  | p1c < p2c = rightRound
  | otherwise = leftRound
  where
    recp1 = take p1c p1cs
    recp2 = take p2c p2cs
    seen' = S.insert (p1, p2) seen
    leftRound = playRec (p1cs ++ [p1c, p2c]) p2cs seen'
    rightRound = playRec p1cs (p2cs ++ [p2c, p1c]) seen'

score cs = sum $ zipWith (*) [1..] (reverse cs)


day22 :: IO ()
day22 = do
  input <- readFile "data/22.txt"
  let [p1, p2] = tail . fmap read <$> (splitOn [""] $ lines input)
      end = head $ dropWhile (\(p1, p2) -> not (null p1) && not (null p2)) $ iterate playRound (p1, p2)
  print $ (score (fst end), score (snd end))
  print $ bimap score score (playRec p1 p2 S.empty)
  pure ()
