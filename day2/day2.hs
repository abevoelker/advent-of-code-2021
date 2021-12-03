subMove :: (Int, Int) -> (String, Int) -> (Int, Int)
subMove x (y,z) = subMove' x y z

subMove' :: (Int, Int) -> String -> Int -> (Int, Int)
subMove' (x,y) "forward" distance = (x + distance, y)
subMove' (x,y) "down"    distance = (x,            y + distance)
subMove' (x,y) "up"      distance = (x,            y - distance)

parseInstructionLine :: String -> (String, Int)
parseInstructionLine x = (head (words x), read (last (words x))::Int)

main = do
  contents <- readFile "input.txt"
  let instructions = map parseInstructionLine (lines contents)
  let finalPosition = foldl subMove (0,0) instructions
  return $ fst finalPosition * snd finalPosition
