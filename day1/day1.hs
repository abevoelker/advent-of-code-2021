depthIncreaseCount :: [Int] -> Int
depthIncreaseCount xs = length $ filter (\x -> snd x > fst x ) (pairs xs) where
  pairs [] = []
  pairs xs = zip xs (tail xs)

main = do
  contents <- readFile "input.txt"
  return $ depthIncreaseCount $ map (\x -> read x::Int) (lines contents)
