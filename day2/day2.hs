-- Part 1

subMove :: (Int, Int) -> (String, Int) -> (Int, Int)
subMove x (y,z) = subMove' x y z

subMove' :: (Int, Int) -> String -> Int -> (Int, Int)
subMove' (x,y) "forward" distance = (x + distance, y)
subMove' (x,y) "down"    distance = (x,            y + distance)
subMove' (x,y) "up"      distance = (x,            y - distance)

parseInstructionLine :: String -> (String, Int)
parseInstructionLine x = (head (words x), read (last (words x))::Int)

part1 = do
  contents <- readFile "input.txt"
  let instructions = map parseInstructionLine (lines contents)
  let finalPosition = foldl subMove (0,0) instructions
  return $ fst finalPosition * snd finalPosition

-- Part 2

subMoveWithAim :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
subMoveWithAim x (y,z) = subMoveWithAim' x y z

subMoveWithAim' :: (Int, Int, Int) -> String -> Int -> (Int, Int, Int)
subMoveWithAim' (x,y,aim) "forward" distance = (x + distance, y + (aim * distance), aim)
subMoveWithAim' (x,y,aim) "down"    distance = (x,            y,                    aim + distance)
subMoveWithAim' (x,y,aim) "up"      distance = (x,            y,                    aim - distance)

part2 = do
  contents <- readFile "input.txt"
  let instructions = map parseInstructionLine (lines contents)
  let finalPosition = foldl subMoveWithAim (0,0,0) instructions
  let (finalX, finalY, _) = finalPosition
  return $ finalX * finalY
