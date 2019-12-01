main :: IO ()
main = print . sum . map (fuel . read) . lines =<< readFile "input.txt"

fuel :: Integer -> Integer
fuel mass = if f <= 0 then 0 else f + fuel f 
    where f = (flip (-) 2 . flip div 3) mass