modifyCell :: (Bool -> Bool) -- ^ Function to apply if valid cell
           -> (Int,Int)      -- ^ Corner min coords
           -> (Int,Int)      -- ^ Corner max coords
           -> Int            -- ^ Row size
           -> Bool           -- ^ Cell to modify
           -> Int            -- ^ Cell position
           -> Bool
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

parseToFunc :: [String] -> Bool -> Int -> Bool
parseToFunc (x:y:rest)
    | x == "toggle"            = runRest not (y:rest)
    | (x,y) == ("turn", "on")  = runRest (const True) rest
    | (x,y) == ("turn", "off") = runRest (const False) rest
    | otherwise                = error "You messed up! FIX IT!"
    where
        runRest func [n1,_,n2] = modifyCell func (parseNumbers n1) (parseNumbers n2) rowSize

parseToFuncs :: [String] -> [Bool -> Int -> Bool]
parseToFuncs = map (parseToFunc . spaceSplit)

runOnCell :: [Bool -> Int -> Bool] -> (Bool,Int) -> Bool
runOnCell [] (cell,_) = cell
runOnCell (f:rest) (cell,pos) = runOnCell rest (f cell pos, pos)

calcAllCells :: [Bool -> Int -> Bool] -> [Bool]
calcAllCells funcs = [runOnCell funcs (False, x) | x <- [1 .. 1000000]]

parseToCells :: [String] -> [Bool]
parseToCells = calcAllCells . parseToFuncs

runAll :: [String] -> Int
runAll = length . filter (== True) . parseToCells

main = do
    contents <- readFile "input.txt"
    let result = runAll $ lines contents
    return result
-- answer: 543903