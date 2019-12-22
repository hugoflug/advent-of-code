module AOC6_2(solve, commonSuffixLength) where

import Data.Text (pack, splitOn, unpack)
import Data.Map (Map, fromList, (!?))
import Data.Maybe (maybeToList)

solve :: String -> Int
solve input = 
  let 
    [youPath, sanPath] = rootPath (orbitMap $ lines input) <$> ["YOU", "SAN"] in
  length youPath + length sanPath - (commonSuffixLength youPath sanPath)*2 - 2

orbitMap :: [String] -> Map String String
orbitMap = fromList . fmap ((\l -> (l !! 1, head l)) . split ")")

commonSuffixLength :: Eq a => [a] -> [a] -> Int
commonSuffixLength l1 l2 = length . takeWhile (\(a, b) -> a == b) $ zip (reverse l1) (reverse l2)

rootPath :: Map String String -> String -> [String]
rootPath orbitMap start = 
  case orbitMap !? start of 
    Just super -> start:(rootPath orbitMap super)
    Nothing    -> [start] 

split :: String -> String -> [String]
split s = map unpack . splitOn (pack s) . pack