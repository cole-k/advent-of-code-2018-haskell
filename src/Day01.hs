module Day01
  (solve)
  where

import Data.Set (Set)
import qualified Data.Set as Set

parseFreq :: String -> Int
parseFreq ('+':freq) = read freq
parseFreq ('-':freq) = negate $ read freq

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 freqs = firstRepeat Set.empty freqIters
  where
    freqIters = scanl (+) 0 $ cycle freqs

firstRepeat :: Set Int -> [Int] -> Int
firstRepeat seen (freq:freqs)
  | freq `Set.member` seen = freq
  | otherwise = firstRepeat (Set.insert freq seen) freqs

solve :: FilePath -> IO ()
solve fp = do
  input <- readFile fp
  let freqs = map parseFreq . lines $ input
  print $ part1 freqs
  print $ part2 freqs
