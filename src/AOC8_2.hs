module AOC8_2(solve) where

import Data.List.Split
import Data.List
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)

solve :: Int -> Int -> String -> IO ()
solve width height = traverse_ putStrLn . chunksOf width . fmap paint . picture . chunksOf (width * height)
  where
    paint '0' = ' '
    paint '1' = 'X'

picture :: [String] -> String
picture layers = if null (head layers) then [] else pixel:(picture (tail <$> layers)) 
  where pixel = fromMaybe '2' . find ((/=) '2') . fmap head $ layers
