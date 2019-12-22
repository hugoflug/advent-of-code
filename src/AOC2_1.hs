module AOC2_1(solve) where

import Data.Sequence
import Data.Text(pack, unpack, splitOn)

solve :: String -> Int
solve = flip index 0 . interpret 0 . restore . fromList . map read . split ","

restore :: Seq Int -> Seq Int
restore = update 1 12 . update 2 2

interpret :: Int -> Seq Int -> Seq Int
interpret i program =
    case index program i of
        1 -> instruct (+)
        2 -> instruct (*)
        99 -> program
    where 
        instruct op = interpret (i + 4) $ update (off 3) ((ix . off $ 1) `op` (ix . off $ 2)) program
        ix = index program
        off n = ix $ i + n

split :: String -> String -> [String]
split s = map unpack . splitOn (pack s) . pack