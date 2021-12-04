-- Part 1

mostCommonBit :: [Char] -> Char
mostCommonBit xs = if (zeroCount > oneCount) then '0' else '1' where
  zeroCount = length $ filter (\x -> x == '0') xs
  oneCount = length $ filter (\x -> x == '1') xs

leastCommonBit :: [Char] -> Char
leastCommonBit xs = if (mostCommonBit xs) == '0' then '1' else '0'

bitToDec :: Char -> Int -> Int
bitToDec '0' _ = 0
bitToDec '1' x = 2 ^ x

bitsToDec :: String -> Int
bitsToDec xs = go (reverse xs) 0 where
  go (x:xs) i = (bitToDec x i) + go xs (i + 1)
  go []     i = 0

combineBits :: [[String]] -> [String]
combineBits x = combineBits' x (cycle [[]])

combineBits' :: [[String]] -> [String] -> [String]
combineBits' [] acc     = acc
combineBits' (x:xs) acc = combineBits' xs (zipWith (++) acc x)

split :: String -> [String]
split = map (\x -> [x])

part1 = do
  contents <- readFile "input.txt"
  let words = combineBits $ map split (lines contents)
  let gamma = bitsToDec $ map mostCommonBit words
  let epsilon = bitsToDec $ map leastCommonBit words
  return $ gamma * epsilon

-- Part 2

oxygenBitmask :: [String] -> String
oxygenBitmask words = map mostCommonBit words

co2Bitmask :: [String] -> String
co2Bitmask words = map leastCommonBit words

filterMask :: ([String] -> String) -> [String] -> String
filterMask f x = filterMask' f x 0

filterMask' :: ([String] -> String) -> [String] -> Int -> String
filterMask' f words currentBit
  | length words == 1 = words !! 0
  | otherwise         = filterMask' f filteredWords (currentBit + 1)
  where
    mask = f . combineBits $ map split words
    filteredWords = filter (\x -> (x !! currentBit) == (mask !! currentBit)) words

part2 = do
  contents <- readFile "input.txt"
  let words = lines contents
  let oxygenRating = bitsToDec $ filterMask oxygenBitmask words
  let co2Rating = bitsToDec $ filterMask co2Bitmask words
  return $ oxygenRating * co2Rating
