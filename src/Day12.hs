{-# LANGUAGE RecordWildCards #-}
module Day12
  (solve)
  where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Foldable (toList)

import Debug.Trace

type Rules = Map String Char

data Plants
  = Plants
  { plants :: Seq Char
  , minBound :: Int
  , maxBound :: Int}
  deriving (Show, Eq, Ord)

mkRules :: [String] -> Map String Char
mkRules = foldr (\line m -> Map.insert (take 5 line) (last line) m) Map.empty

parsePlants :: [String] -> (Rules, Plants)
parsePlants ls = (rules, Plants ps 0 (length ps - 1))
  where
    gen0 = drop 15 $ head ls
    ps = Seq.fromList gen0
    rules = mkRules $ drop 2 ls

sections :: Int -> [a] -> [[a]]
sections _ [] = []
sections n xs@(_:xs') =
  if length section == n
    then section : sections n xs'
    else []
  where
    section = take n xs

findPlant :: Int -> Plants -> Char
findPlant i Plants{..} = case Seq.lookup i plants of
  Just p  -> p
  Nothing -> '.'

plantIndices :: Int -> Int -> [Char] -> [Int]
plantIndices minBound maxBound plants
  = [i | (i, '#') <- zip [minBound .. maxBound] plants]

updatePlants :: Rules -> Plants -> Plants
updatePlants rules ps@Plants{..} = Plants plants' minBound' maxBound'
  where
    plantSections = sections 5 $ "...." ++ toList plants ++ "...."
    nextGeneration = [Map.findWithDefault '.' ps rules | ps<-plantSections]
    -- Remove trailing and leading dots.
    nextGeneration' = take (1 + maxBound' - minBound') . drop (minBound' - (minBound - 2)) $
      nextGeneration
    plants' = Seq.fromList nextGeneration'
    inds = plantIndices (minBound - 2) (maxBound + 2) nextGeneration
    minBound' = case inds of
                  []    -> 0
                  (i:_) -> i
    maxBound' = case inds of
                  [] -> 0
                  is -> last is

generations :: Rules -> Plants -> [Plants]
generations rules gen0 = iterate (updatePlants rules) gen0

generation :: Rules -> Plants -> Int -> Plants
generation rules gen0 n = generations rules gen0 !! n

sumLiving :: Plants -> Int
sumLiving Plants{..} = sum $ plantIndices minBound maxBound (toList plants)

part1 :: Rules -> Plants -> Int
part1 rules gen0 = sumLiving $ generation rules gen0 20

part2 :: Rules -> Plants -> Int
part2 rules gen0 = sumLiving $ generation rules gen0 50000000000

-- indexOfFirstDuplicate :: [Plants] -> Int
-- indexOfFirstDuplicate ps = indexOfFirstDuplicate' Map.empty (zip [0..] ps)

-- indexOfFirstDuplicate' :: Map Plants Int -> [(Int, Plants)] -> Int
-- indexOfFirstDuplicate' seen ((i,ps):ps')
--   | Just ind <- Map.lookup ps seen = ind
--   | otherwise = indexOfFirstDuplicate' (Map.insert ps i seen) ps'

solve :: FilePath -> IO ()
solve file = do
  input <- lines <$> readFile file
  let (rules, ps) = parsePlants input
  print $ part1 rules ps
