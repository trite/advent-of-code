{-# ANN module "HLint: ignore Eta reduce" #-}


splitOn :: Char -> String -> [String]
splitOn = splitOn' [] []
    where
        splitOn' :: [String] -> String -> Char -> String -> [String]
        splitOn' accList acc _ [] = reverse $ reverse acc:accList
        splitOn' accList acc split (x:rest)
            | split == x = splitOn' (reverse acc:accList) [] split rest
            | otherwise  = splitOn' accList (x:acc) split rest

testList = lines "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
-- >>> testList
-- ["forward 5","down 5","forward 8","up 3","down 8","forward 2"]

data Position = Position
    { hor :: Int
    , dep :: Int
    , aim :: Int }

toString pos@Position {hor = h, dep = d, aim = a} =
    "Horizontal position: " ++ show h ++
    " Depth: " ++ show d ++
    " Calculated result: " ++ show (h*d) ++
    " Aim: " ++ show a

toInt str = read str :: Int

process :: String -> String -> Position -> Position
process "forward" x pos@Position { hor = h, dep = d, aim = a } =
    pos { hor = h + toInt x, dep = d + (a * toInt x) }
process "down"    x pos@Position { aim = a } =
    pos { aim = a + toInt x }
process "up"      x pos@Position { aim = a } =
    pos { aim = a - toInt x }
process _ _ _ = error "Missed something with processing"

processList' :: [[String]] -> [Position]
processList' list = run Position {hor = 0, dep = 0, aim = 0} list []
    where
        run :: Position -> [[String]] -> [Position] -> [Position]
        run pos [] acc = acc
        run pos ([way,dist]:rest) acc = run result rest (result:acc)
            where
                result :: Position
                result = process way dist pos
        run _ _ _ = error "Missed something with processList"

processList list = head $ processList' list

main = do
    contents <- readFile "input.txt"
    let result = toString $ processList $ map (splitOn ' ') $ lines contents
    return result
-- Answer: 1544000595 -- "Horizontal position: 1905 Depth: 810499 Calculated result: 1544000595 Aim: 907"






-- >>> map toString $ processList $ map (splitOn ' ') testList
{-
-- [
    "Horizontal position: 15 Depth: 60 Calculated result: 900 Aim: 10",
    "Horizontal position: 13 Depth: 40 Calculated result: 520 Aim: 10",
    "Horizontal position: 13 Depth: 40 Calculated result: 520 Aim: 2",
    "Horizontal position: 13 Depth: 40 Calculated result: 520 Aim: 5",
    "Horizontal position: 5 Depth: 0 Calculated result: 0 Aim: 5",
    "Horizontal position: 5 Depth: 0 Calculated result: 0 Aim: 0"]

    // Accidentally calculating with horizontal instead of aim: dep = d + (h * toInt x)
    "Horizontal position: 15 Depth: 66 Calculated result: 990 Aim: 10",
    "Horizontal position: 13 Depth: 40 Calculated result: 520 Aim: 10",
    "Horizontal position: 13 Depth: 40 Calculated result: 520 Aim: 2",
    "Horizontal position: 13 Depth: 40 Calculated result: 520 Aim: 5",
    "Horizontal position: 5 Depth: 0 Calculated result: 0 Aim: 5",
    "Horizontal position: 5 Depth: 0 Calculated result: 0 Aim: 0"]
-}
