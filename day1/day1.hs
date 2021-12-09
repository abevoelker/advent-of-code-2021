import Data.List (transpose)

depthIncreaseCount :: [Int] -> Int
depthIncreaseCount xs = length $ filter (\x -> snd x > fst x ) (pairs xs) where
  pairs [] = []
  pairs xs = zip xs (tail xs)

part1 = do
  contents <- readFile "input.txt"
  let depths = map (\x -> read x::Int) (lines contents)
  return $ depthIncreaseCount depths

part2 = do
  contents <- readFile "input.txt"
  let depths = map (\x -> read x::Int) (lines contents)
  let slidingWindow = transpose [depths, (drop 1 depths), (drop 2 depths)]
  let slidingWindowSums = map sum $ filter (\x -> length x == 3) slidingWindow
  return $ depthIncreaseCount slidingWindowSums
