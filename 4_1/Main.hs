main :: IO ()
main = print $ checkAll 245182 790572

checkAll :: Int -> Int -> Int
checkAll from to = length . filter check $ digits <$> [from..to]

check :: [Int] -> Bool
check = check' False
  where
    check' hasPair (h1:h2:tail)
      | h2 > h1 = check' hasPair (h2:tail)
      | h1 == h2 = check' True (h2:tail)
      | h2 < h1 = False
    check' hasPair (_:[]) = hasPair
    check' hasPair [] = hasPair

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]