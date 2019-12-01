main :: IO ()
main = print . sum . map (flip (-) 2 . flip div 3 . read) . lines =<< readFile "input.txt"

