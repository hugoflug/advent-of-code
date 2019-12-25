module AOC8_1(solve) where

import Data.List.Split
import Data.List

solve :: String -> Int
solve image = count '1' layer * count '2' layer
    where layer = head . sortOn (count '0') . chunksOf (25*6) $ image

count :: Eq a => a -> [a] -> Int
count x = length . filter ((==) x)