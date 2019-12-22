module AOC2_2(solve) where

import Data.Sequence(fromList, update, Seq(..), index)
import Data.Text (pack, splitOn, unpack)
import Data.List(elemIndex)
import Data.Maybe(fromJust)

solve :: String -> Int
solve = fromJust . findResult 19690720 . map read . split ","

findResult :: Int -> [Int] -> Maybe Int
findResult goal input =
  elemIndex goal $ tryPair input <$> [0..99] <*> [0..99]

tryPair :: [Int] -> Int -> Int -> Int
tryPair input a b =
  flip index 0 . interpret 0 . restore a b . fromList $ input

restore :: Int -> Int -> Seq Int -> Seq Int
restore a b = update 1 a . update 2 b

interpret :: Int -> Seq Int -> Seq Int
interpret i program =
  case index program i of
    1 -> instruct (+)
    2 -> instruct (*)
    99 -> program
  where
    instruct op =
      interpret (i + 4) $
      update (off 3) ((ix . off $ 1) `op` (ix . off $ 2)) program
    ix = index program
    off n = ix $ i + n

split :: String -> String -> [String]
split s = map unpack . splitOn (pack s) . pack
