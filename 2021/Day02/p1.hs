{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

process :: String -> String -> Pos -> Pos
process "forward" x pos@Pos {hor = h} = pos { hor = h + (read x :: Int)}
process "down"    x pos@Pos {dep = d} = pos { dep = d - (read x :: Int)}
process "up"      x pos@Pos {dep = d} = pos { dep = d + (read x :: Int)}
process _ _ _ = error "Missed something with processing"

-- processList list = -- TODO: Leaving off here for the night

main = do
    contents <- readFile "input.txt"
    let result = lines contents
    return result
