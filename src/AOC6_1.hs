module AOC6_1(solve) where

import Data.Text (pack, splitOn, unpack)
import Data.Map (Map, fromListWith, (!?))
import Data.Maybe (maybeToList)

solve :: String -> Int
solve = orbits . orbitMap . lines 

orbitMap :: [String] -> Map String [String]
orbitMap = fromListWith (++) . fmap ((\l -> (head l, tail l)) . split ")")

orbits :: Map String [String] -> Int
orbits orbitMap = orbits' orbitMap 0 "COM"
  where 
    orbits' orbitMap supers name = 
      supers + case orbitMap !? name of
        Nothing         -> 0
        Just subOrbits  -> sum $ orbits' orbitMap (supers + 1) <$> subOrbits

split :: String -> String -> [String]
split s = map unpack . splitOn (pack s) . pack