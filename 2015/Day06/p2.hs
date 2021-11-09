{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

modifyCell :: (Int -> Int) -- ^ Function to apply if valid cell
           -> (Int,Int)      -- ^ Corner min coords
           -> (Int,Int)      -- ^ Corner max coords
           -> Int            -- ^ Row size
           -> Int           -- ^ Cell to modify
           -> Int            -- ^ Cell position
           -> Int
modifyCell func start end rowSize cell pos
    | isInRange rowSize start end pos = func cell
    | otherwise                       = cell
    where
        isInRange rowSize (sx,sy) (ex,ey) pos =
            sx <= (pos `div` rowSize) &&
            (pos `div` rowSize) <= ex &&
            sy <= (pos `mod` rowSize) &&
            (pos `mod` rowSize) <= ey

splitOn :: Char -> String -> [String]
splitOn = splitOn' [] []
    where
        splitOn' :: [String] -- ^ accList - previous accumulations
            -> String        -- ^ acc - current accumulator
            -> Char          -- ^ split - character to split on
            -> String        -- ^ rest of the string
            -> [String]
        splitOn' accList acc _ [] = reverse $ reverse acc:accList
        splitOn' accList acc split (x:rest)
            | split == x = splitOn' (reverse acc:accList) [] split rest
            | otherwise  = splitOn' accList (x:acc) split rest

parseNumbers :: String -> (Int, Int)
parseNumbers = pair . splitOn ','
    where pair [x,y] = (read x, read y)

rowSize :: Int
rowSize = 1000

spaceSplit :: String -> [String]
spaceSplit = splitOn ' '

-- turnOff x
--     | x - 1 >= 0 = x - 1
--     | otherwise  = 0

turnOff :: Int -> Int
turnOff x = max 0 x-1

parseToFunc :: [String] -> Int -> Int -> Int
parseToFunc (x:y:rest)
    | x == "toggle"            = runRest (+2) (y:rest)
    | (x,y) == ("turn", "on")  = runRest (+1) rest
    | (x,y) == ("turn", "off") = runRest turnOff rest
    | otherwise                = error "You messed up! FIX IT!"
    where
        runRest func [n1,_,n2] = modifyCell func (parseNumbers n1) (parseNumbers n2) rowSize

parseToFuncs :: [String] -> [Int -> Int -> Int]
parseToFuncs = map (parseToFunc . spaceSplit)

runOnCell :: [Int -> Int -> Int] -> (Int,Int) -> Int
runOnCell [] (cell,_) = cell
runOnCell (f:rest) (cell,pos) = runOnCell rest (f cell pos, pos)

calcAllCells :: [Int -> Int -> Int] -> [Int]
calcAllCells funcs = [runOnCell funcs (0, x) | x <- [1 .. 1000000]]

parseToCells :: [String] -> [Integer]
parseToCells str = map toInteger (calcAllCells . parseToFuncs $ str)

runAll :: [String] -> Integer
runAll = sum . parseToCells



main = do
    contents <- readFile "input.txt"
    let result = runAll $ lines contents
    return result
-- answer: 14687245