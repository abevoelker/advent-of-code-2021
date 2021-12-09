module Main where
import Data.List.Split
import Data.Map (toList, fromListWith)

-- Part 1

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine x = ((y !! 0, y !! 1), (y !! 2, y !! 3)) where --(pairs nums !! 0, pairs nums !! 1) where
  y = map (\x -> read x::Int) $ concat $ map (splitOn ",") $ (splitOn " -> " x)

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween (x,y) (x',y')
  | x == x' || y == y'  = [(i,j) | i <- [(min x x')..(max x x')],
                                   j <- [(min y y')..(max y y')] ]
  | otherwise           = []

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let l = map parseLine (lines contents)
  let allPoints = concat $ map (\x -> pointsBetween (fst x) (snd x)) l
  let pointCounts = toList $ fromListWith (+) [(c, 1) | c <- allPoints]
  print $ length $ filter (\(_,x) -> x >= 2) pointCounts
