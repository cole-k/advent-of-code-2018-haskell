module Day05
  (solve)
  where

import Data.Char (toLower)

reduce :: String -> String
reduce (x:y:zs)
  | toLower x == toLower y && x /= y = reduce zs
  | otherwise = x : reduce (y : zs)
reduce xs = xs

converge :: Eq a => [a] -> Maybe a
converge (x:y:zs)
  | x == y = Just x
  | otherwise = converge $ y : zs
converge [] = Nothing

part1 :: String -> Maybe Int
part1 xs = do
  res <- converge . iterate reduce $ xs
  return $ length res

part2 :: String -> Maybe Int
part2 xs = do
  reduced <- converge . iterate reduce $ xs
  results <- sequence $ possibleLengths reduced
  return $ minimum results
  where
    possibleLengths xs = do
      l <- ['a'..'z']
      let res = part1 $ filter (\c -> toLower c /= l) xs
      return res

solve :: FilePath -> IO ()
solve file = do
  s <- readFile file
  maybe (print "Part 1 Failed.") print $ part1 s
  maybe (print "Part 2 Failed.") print $ part2 s
