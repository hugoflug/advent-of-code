module AOC3_1(solve) where

import Data.Text (pack, splitOn, unpack)
import Data.List (sortOn)
import Data.Set (Set, fromList, toList, intersection, union, empty)

data Instruction =
  Instruction Direction Int
  deriving (Eq, Show)

data Direction = 
  U | R | D | L
  deriving (Eq, Show)

type Coord = (Int, Int)

solve :: String -> Int
solve = uncurry closestDist . parse

parse :: String -> ([Instruction], [Instruction])
parse input = 
  let wires = split "\n" input
      w1 = head wires
      w2 = last wires 
      parseLine l = parseInstruction <$> split "," l in 
      (parseLine w1, parseLine w2)

split :: String -> String -> [String]
split s = map unpack . splitOn (pack s) . pack

parseInstruction :: String -> Instruction
parseInstruction (dir:n) = Instruction (parseDir dir) (read n)
  where 
    parseDir d = case d of
      'U' -> U
      'R' -> R
      'D' -> D
      'L' -> L

closestDist :: [Instruction] -> [Instruction] -> Int
closestDist i1 i2 = manhattan $ closest i1 i2

closest :: [Instruction] -> [Instruction] -> Coord
closest i1 i2 = head . sortOn manhattan . toList $ intersection (coords i1) (coords i2)

manhattan :: Coord -> Int
manhattan (x, y) = (abs x) + (abs y)

coords :: [Instruction] -> Set Coord
coords = fst . foldl accCoords (empty, (0, 0))
  where 
    accCoords (acc, cur) instr = 
      let newCoords = coordsFrom cur instr in
        (union acc . fromList $ newCoords, last newCoords)

coordsFrom :: Coord -> Instruction -> [Coord]
coordsFrom coord (Instruction direction n) = move direction coord <$> [1 .. n]

move :: Direction -> Coord -> Int -> Coord
move U (x, y) n = (x, y - n)
move D (x, y) n = (x, y + n)
move R (x, y) n = (x + n, y)
move L (x, y) n = (x - n, y)
