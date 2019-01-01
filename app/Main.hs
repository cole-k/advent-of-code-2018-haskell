module Main where

import System.Console.ArgParser
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import qualified Day01
import qualified Day02
import qualified Day05
import qualified Day12

data ParseArgs = ParseArgs Int FilePath
  deriving (Show)

solutions :: HashMap Int (FilePath -> IO ())
solutions = Map.fromList
  [ ( 1, Day01.solve)
  , ( 2, Day02.solve)
  , ( 5, Day05.solve)
  , (12, Day12.solve)]

argParser :: ParserSpec ParseArgs
argParser = ParseArgs
  `parsedBy` reqPos "day" `Descr` "day number"
  `andBy` optPos "" "filename" `Descr` "file containing problem input"

main :: IO ()
main = withParseResult argParser solveDay

solveDay :: ParseArgs -> IO ()
solveDay (ParseArgs day fileArg) =
  case Map.lookup day solutions of
    -- The function exists
    Just solve -> do
      solve file
    -- The function does not exist
    Nothing -> putStrLn $ "Day number " ++ show day ++ " doesn't exist"
  where
    file = case fileArg of
      -- Supply default problem file
      "" ->
        if day < 10
           -- Front pad with 0 for single digit days
           then concat ["in/0", show day, ".in"]
           else concat ["in/", show day, ".in"]
      otherwise -> fileArg
