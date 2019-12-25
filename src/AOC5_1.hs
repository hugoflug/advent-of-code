module AOC5_1(solve, runInterpreter) where

import Data.Sequence(fromList, update, Seq(..), index)
import Data.Text (pack, splitOn, unpack)
import Control.Monad.Writer

solve :: String -> Int
solve = last . runInterpreter
  
runInterpreter :: String -> [Int]
runInterpreter = execWriter . interpret 0 . fromList . map read . split ","

arithmetic :: (Int -> Int -> Int) -> Int -> Int -> Int -> Seq Int -> Seq Int
arithmetic op a b c program = update c (a `op` b) program

readInput :: Int -> Seq Int -> Seq Int
readInput n program = update n 1 program

writeOutput :: Int -> Seq Int -> Writer [Int] (Seq Int)
writeOutput a program = tell [a] >> return program

interpret :: Int -> Seq Int -> Writer [Int] (Seq Int)
interpret ix program =
  let 
    [a3mode, a2mode, a1mode, op1, op2] = digits5 (off 0) 
    [a1, a2, _] = uncurry param <$> [a1mode, a2mode, a3mode] `zip` [1..3]
    [d1, _, d3] = off <$> [1..3]
    op = undigits [op1, op2]
    param mode n = if mode == 1 then off n else index program (off n)
    off n = index program (ix + n)
  in
    case op of
      1 -> interpret (ix + 4) (arithmetic (+) a1 a2 d3 program)
      2 -> interpret (ix + 4) (arithmetic (*) a1 a2 d3 program)
      3 -> interpret (ix + 2) (readInput d1 program)
      4 -> interpret (ix + 2) =<< writeOutput a1 program
      99 -> return program

undigits :: Integral x => [x] -> x
undigits = undigits' . reverse
  where
    undigits' [] = 0
    undigits' (h:t) = h + 10*(undigits' t)

digits5 :: Integral x => x -> [x]
digits5 = digits 5
    where
      digits n 0 = replicate n 0
      digits n x = digits (n - 1) (x `div` 10) ++ [x `mod` 10]

split :: String -> String -> [String]
split s = map unpack . splitOn (pack s) . pack
