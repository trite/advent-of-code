-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.Show.Functions

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

-- >>> map (splitOn ' ') testList
-- [["forward","5"],["down","5"],["forward","8"],["up","3"],["down","8"],["forward","2"]]

data Pos = Pos { hor :: Int
               , dep :: Int } 

toString pos@Pos {hor = h, dep = d} = "Horizontal position: " ++ show h ++ " Depth: " ++ show d ++ " Calculated result: " ++ show (h*d)

-- instance Show Pos where
--     show {hor, dep} = show "hor: " ++ hor ++ " dep: " ++ dep

-- process :: String -> String -> Pos -> Pos
-- process "forward" x pos@Pos {hor = h} = pos {hor = h + (read x :: Int)}
-- process "down"    x pos@Pos {dep = d} = pos {dep = d + (read x :: Int)}
-- process "up"      x pos@Pos {dep = d} = pos {dep = d - (read x :: Int)}
-- process _ _ _ = error "Missed something with processing"

process :: String -> Int -> Pos -> Pos
process "forward" x pos@Pos {hor = h} = pos {hor = h + x}
process "down"    x pos@Pos {dep = d} = pos {dep = d + x}
process "up"      x pos@Pos {dep = d} = pos {dep = d - x}
process _ _ _ = error "Missed something with processing"

processList :: [[String]] -> Pos
processList list = run Pos {hor = 0, dep = 0} list 
    where
        run pos [] = pos
        run pos ([way,dist]:rest) = run (process way (read dist :: Int) pos) rest
        run _ _ = error "Missed something with processList"
-- >>> toString $ processList $ map (splitOn ' ') testList
-- "Horizontal position: 15 Depth: 10 Calculated result: 150"


main = do
    contents <- readFile "input.txt"
    let result = toString $ processList $ map (splitOn ' ') $ lines contents
    return result
-- Answer: 1727835 -- "Horizontal position: 1905 Depth: 907 Calculated result: 1727835"
