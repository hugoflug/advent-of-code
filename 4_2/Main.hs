import Data.List(group, any)

main :: IO ()
main = print $ checkAll 245182 790572

checkAll :: Int -> Int -> Int
checkAll from to = length . filter check $ digits <$> [from..to]

check :: [Int] -> Bool
check list = ascending list && (any ((==) 2 . length) . group) list 

ascending :: [Int] -> Bool
ascending (h1:h2:tail)
  | h2 < h1 = False
  | otherwise = ascending (h2:tail)
ascending (_:[]) = True
ascending [] = True

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]