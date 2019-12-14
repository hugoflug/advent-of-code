import Data.Text (pack, splitOn, unpack)
import Data.List (intersect, sortOn)

data Instruction =
  Instruction Direction Int
  deriving (Eq, Show)

data Direction = 
  U | R | D | L
  deriving (Eq, Show)

type Coord = (Int, Int)

main = print . closestDist' . parse =<< readFile "input.txt"
  where 
    closestDist' (i1, i2) = closestDist i1 i2

parse :: String -> ([Instruction], [Instruction])
parse input = 
  let wires = split "\n" input
      w1 = head wires
      w2 = last wires in 
      (parseInstruction <$> split "," w1, parseInstruction <$> split "," w2)

parseInstruction :: String -> Instruction
parseInstruction (dir:n) = 
  Instruction (parseDir dir) (read n)
  where 
    parseDir d = case d of
      'U' -> U
      'R' -> R
      'D' -> D
      'L' -> L

closestDist :: [Instruction] -> [Instruction] -> Int
closestDist instrs = manhattan . closest instrs

closest :: [Instruction] -> [Instruction] -> Coord
closest instrs = head . sortOn manhattan . intersect (coords instrs) . coords

manhattan :: Coord -> Int
manhattan (x, y) = (abs x) + (abs y)

coords :: [Instruction] -> [Coord]
coords instructions = fst $ foldl accCoords ([], (0, 0)) instructions

accCoords :: ([Coord], Coord) -> Instruction -> ([Coord], Coord)
accCoords (acc, cur) instr = (acc ++ newCoords, last newCoords)
  where
    newCoords = coordsFrom cur instr

coordsFrom :: Coord -> Instruction -> [Coord]
coordsFrom coord (Instruction direction n) = move direction coord <$> [1 .. n]

move :: Direction -> Coord -> Int -> Coord
move U (x, y) n = (x, y - n)
move D (x, y) n = (x, y + n)
move R (x, y) n = (x + n, y)
move L (x, y) n = (x - n, y)

split :: String -> String -> [String]
split s = map unpack . splitOn (pack s) . pack