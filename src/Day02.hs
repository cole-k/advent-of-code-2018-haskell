module Day02
  (solve)
  where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (catMaybes)
import Control.Monad (guard)

mkCount :: String -> Map Char Int
mkCount = foldr go Map.empty
  where
    go char count = Map.insertWith (+) char 1 count

part1 :: [String] -> Int
part1 ss = numTwos * numThrees
  where
    counts = map (Map.elems . mkCount) ss
    numTwos   = length [c | c<-counts, any (==2) c]
    numThrees = length [c | c<-counts, any (==3) c]

match :: Eq a => [a] -> [a] -> [a]
match s1 s2 = catMaybes $
  zipWith (\a b -> if a == b then Just a else Nothing) s1 s2

part2 :: [String] -> String
part2 ids@(firstId:_) = head $ do
  id1 <- ids
  id2 <- ids
  let matching = match id1 id2
  -- match at all but 1 location
  guard $ length matching == idLength - 1
  return matching
  where
    idLength = length firstId

solve :: FilePath -> IO ()
solve file = do
  input <- lines <$> readFile file
  print $ part1 input
  putStrLn $ part2 input
