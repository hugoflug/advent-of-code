module AOC3_2(solve) where

import Data.Text (pack, splitOn, unpack)
import Data.List (sort)
import Data.Set (Set, fromList, union, empty)
import Data.Map (fromSet, toList, intersectionWith)

data Instruction =
  Instruction Direction Int
  deriving (Eq, Show)

data Direction = 
  U | R | D | L
  deriving (Eq, Show)

data Coord = 
  Coord {
    coord :: (Int, Int),
    steps :: Int
  }
  deriving Show

instance Eq Coord where
  c1 == c2 = coord c1 == coord c2

instance Ord Coord where
  c1 <= c2 = coord c1 <= coord c2

solve :: String -> Int
solve = uncurry closest . parse

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

closest :: [Instruction] -> [Instruction] -> Int
closest i1 i2 = head . sort . map snd . toList $ intersectionWith (+) (stepMap i1) (stepMap i2)
  where stepMap = fromSet steps . coords

coords :: [Instruction] -> Set Coord
coords = fst . foldl accCoords (empty, Coord (0, 0) 0)
  where 
    accCoords (acc, cur) instr = 
      let newCoords = coordsFrom cur instr in
        (union acc . fromList $ newCoords, last newCoords)

coordsFrom :: Coord -> Instruction -> [Coord]
coordsFrom coord (Instruction direction n) = move direction coord <$> [1 .. n]

move :: Direction -> Coord -> Int -> Coord
move dir (Coord (x, y) s) n = Coord newCoord (s + n)
  where 
    newCoord = case dir of
      U -> (x, y - n)
      D -> (x, y + n)
      R -> (x + n, y)
      L -> (x - n, y)