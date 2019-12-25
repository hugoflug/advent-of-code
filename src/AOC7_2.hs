{-# LANGUAGE FlexibleContexts #-}

module AOC7_2(solve, runAmplifier) where

import Data.Sequence(fromList, update, Seq(..), index)
import Data.Text (pack, splitOn, unpack)
import Control.Monad.Writer
import Control.Monad.State
import Data.List (permutations)
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Parallel.Strategies
import Control.Concurrent

type Program = Seq Int

solve :: String -> IO Int
solve program = maximum <$> (traverse (runAmplifiers program 0) . permutations) [5..9]

atomicWrite :: Show a => TChan a -> a -> IO ()
atomicWrite chan  = atomically . writeTChan chan 

atomicRead :: TChan a -> IO a
atomicRead = atomically . readTChan

runAmplifiers :: String -> Int -> [Int] -> IO Int
runAmplifiers program startSignal phaseSettings = do
  inChan <- newTChanIO
  outChan <- foldM (\chan setting -> runAndSetAmplifier program setting chan) inChan (init phaseSettings)
  finishedChan <- newTChanIO
  runAmplifier program (last phaseSettings) outChan inChan finishedChan
  atomicWrite inChan startSignal
  _ <- atomicRead finishedChan
  atomicRead inChan
  where 
    runAndSetAmplifier program phaseSetting inChan = do
      outChan <- newTChanIO
      ignored <- newTChanIO
      runAmplifier program phaseSetting inChan outChan ignored
      return outChan

runAmplifier :: String -> Int -> TChan Int -> TChan Int -> TChan () -> IO ThreadId
runAmplifier program phaseSetting inChan outChan finishedChan = do
  atomicWrite inChan phaseSetting
  forkIO . void $ runInterpreter (atomicRead inChan) (atomicWrite outChan) (atomicWrite finishedChan ()) program

runInterpreter :: Monad m => m Int -> (Int -> m ()) -> m () -> String -> m Program
runInterpreter input output finished program =
  (flip evalStateT) 0 $ runInterpreter' (lift input) (lift . output) (lift finished) program
  where runInterpreter' input output finished = interpret input output finished . fromList . map read . split ","

arithmetic :: MonadState Int m => (Int -> Int -> Int) -> Int -> Int -> Int -> Program -> m Program
arithmetic op a b c program =
  return (update c (a `op` b) program) <* modify ((+) 4)

readInput :: MonadState Int m => m Int -> Int -> Program -> m Program
readInput input n program = do
  inp <- input
  modify $ (+) 2
  return $ update n inp program

writeOutput :: MonadState Int m => (Int -> m ()) -> Int -> Program -> m Program
writeOutput output a program = 
  output a >> modify ((+) 2) >> return program

jumpIfTrue :: MonadState Int m => Int -> Int -> Program -> m Program
jumpIfTrue 0 _ program = modify ((+) 3) >> return program
jumpIfTrue _ n program = put n >> return program

jumpIfFalse :: MonadState Int m => Int -> Int -> Program -> m Program
jumpIfFalse 0 n program = put n >> return program
jumpIfFalse _ _ program = modify ((+) 3) >> return program

comp :: MonadState Int m => (Int -> Int -> Bool) -> Int -> Int -> Int -> Program -> m Program
comp compare a b c program = return (update c (if a `compare` b then 1 else 0) program) <$> modify ((+) 4)

interpret :: MonadState Int m => m Int -> (Int -> m ()) -> m () -> Program -> m Program
interpret input output finished program =
  do 
    ix <- get
    let off n = index program (ix + n)
    let param mode n = if mode == 1 then off n else index program (off n)
    let [a3mode, a2mode, a1mode, op1, op2] = digits5 (off 0) 
    let [a1, a2, a3] = uncurry param <$> [a1mode, a2mode, a3mode] `zip` [1..3]
    let [d1, d2, d3] = off <$> [1..3]
    let op = undigits [op1, op2]
    case op of
      99 -> finished >> return program
      _ -> interpret input output finished =<< case op of
        1 -> arithmetic (+) a1 a2 d3 program
        2 -> arithmetic (*) a1 a2 d3 program
        3 -> readInput input d1 program
        4 -> writeOutput output a1 program
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
