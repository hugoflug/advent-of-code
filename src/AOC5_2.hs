{-# LANGUAGE FlexibleContexts #-}

module AOC5_2(solve, runInterpreter) where

import Data.Sequence(fromList, update, Seq(..), index)
import Data.Text (pack, splitOn, unpack)
import Control.Monad.Writer

import Control.Monad.State

type Program = Seq Int

solve :: Int -> String -> Int
solve input = last . runInterpreter input

runInterpreter :: Int -> String -> [Int]
runInterpreter input = (flip evalState) 0 . execWriterT . interpret input . fromList . map read . split ","

arithmetic ::  MonadState Int m => (Int -> Int -> Int) -> Int -> Int -> Int -> Program -> m Program
arithmetic op a b c program =
  return (update c (a `op` b) program) <$> modify ((+) 4)

readInput :: MonadState Int m => Int -> Int -> Program -> m Program
readInput input n program = 
  return (update n input program) <$> modify ((+) 2)

writeOutput :: (MonadWriter [Int] m, MonadState Int m) =>  Int -> Program -> m Program
writeOutput a program = 
  tell [a] >> modify ((+) 2) >> return program

jumpIfTrue :: MonadState Int m => Int -> Int -> Program -> m Program
jumpIfTrue 0 _ program = modify ((+) 3) >> return program
jumpIfTrue _ n program = put n >> return program

jumpIfFalse :: MonadState Int m => Int -> Int -> Program -> m Program
jumpIfFalse 0 n program = put n >> return program
jumpIfFalse _ _ program = modify ((+) 3) >> return program

comp :: MonadState Int m => (Int -> Int -> Bool) -> Int -> Int -> Int -> Program -> m Program
comp compare a b c program = return (update c (if a `compare` b then 1 else 0) program) <$> modify ((+) 4)

interpret :: (MonadWriter [Int] m, MonadState Int m) => Int -> Program -> m Program
interpret input program =
  do 
    ix <- get
    let off n = index program (ix + n)
    let param mode n = if mode == 1 then off n else index program (off n)
    let [a3mode, a2mode, a1mode, op1, op2] = digits5 (off 0) 
    let [a1, a2, a3] = uncurry param <$> [a1mode, a2mode, a3mode] `zip` [1..3]
    let [d1, d2, d3] = off <$> [1..3]
    let op = undigits [op1, op2]
    case op of
      99 -> return program
      _ -> interpret input =<< case op of
        1 -> arithmetic (+) a1 a2 d3 program
        2 -> arithmetic (*) a1 a2 d3 program
        3 -> readInput input d1 program
        4 -> writeOutput a1 program
        5 -> jumpIfTrue a1 a2 program
        6 -> jumpIfFalse a1 a2 program
        7 -> comp (<) a1 a2 d3 program
        8 -> comp (==) a1 a2 d3 program
        e@_ -> error $ "Unknown op code: " <> show e

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
