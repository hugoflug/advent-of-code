module AOC1_1(solve) where

solve :: String -> Int
solve = sum . map (flip (-) 2 . flip div 3 . read) . lines

