module AOC1_2(solve) where

solve :: String -> Int
solve = sum . map (fuel . read) . lines

fuel :: Int -> Int
fuel mass = if f <= 0 then 0 else f + fuel f 
    where f = mass `div` 3 - 2