-- createGrid x y = [ False | x <- [1..x], y <- [1..y] ]

-- createSquareGrid x = createGrid x x


-- type LightChange :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]


-- modifyCell :: Bool           -- ^ Cell to modify
--            -> Int            -- ^ Position of cell
--            -> (Int,Int)      -- ^ min coords to change
--            -> (Int,Int)      -- ^ max coords to change
--            -> (Bool -> Bool) -- ^ Change to make
--            -> Int            -- ^ Row size
--            -> Bool
-- modifyCell cell pos start end func rowSize
--     | isInRange rowSize start end pos = func cell
--     | otherwise                       = cell
--     where
--         isInRange rowSize (sx,sy) (ex,ey) pos =
--             sx <= (pos `div` rowSize) &&
--             (pos `div` rowSize) <= ex &&
--             sy <= (pos `mod` rowSize) &&
--             (pos `mod` rowSize) <= ey 
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

-- >>> map (modifyCell (const True) (2,2) (5,5) 10 False) [0,5..50]
-- [False,False,False,False,False,True,False,True,False,True,False]





splitOn :: Char -> [Char] -> [String]
splitOn = splitOn' [] []
    where
        splitOn' :: [String] -- ^ accList - previous accumulations
            -> [Char]        -- ^ acc - current accumulator
            -> Char          -- ^ split - character to split on
            -> [Char]        -- ^ rest of the string
            -> [String]
        splitOn' accList acc _ [] = reverse $ reverse acc:accList
        splitOn' accList acc split (x:rest)
            | split == x = splitOn' (reverse acc:accList) [] split rest
            | otherwise  = splitOn' accList (x:acc) split rest

parseNumbers :: [Char] -> (Int, Int)
parseNumbers = pair . splitOn ','
    where pair [x,y] = (read x, read y)
-- >>> map parseNumbers ["0,0", "123,234", "999,999"]
-- [(0,0),(123,234),(999,999)]

-- actionDecision (x:y:rest)
--     | x == "toggle"            = "it was toggle"
--     | (x,y) == ("turn", "on")  = "it was turn on"
--     | (x,y) == ("turn", "off") = "it was turn off"
--     | otherwise                = "something else"

-- >>> runLightsChange [False,False,False,False] ["turn", "on", "0,0", "through", "0,1"]
-- [True,True,False,False]

-- runRest' func list [n1,_,n2] = func list (parseNumbers n1) (parseNumbers n2)



rowSize :: Int
rowSize = 1000

-- testGrid = createSquareGrid rowSize

-- testValues = ["turn on 0,0 through 999,999", "toggle 0,0 through 999,0", "turn off 499,499 through 500,500"]

spaceSplit :: [Char] -> [String]
spaceSplit = splitOn ' '

parseToFunc :: [String] -> Bool -> Int -> Bool
parseToFunc (x:y:rest)
    | x == "toggle"            = runRest not (y:rest)
    | (x,y) == ("turn", "on")  = runRest (const True) rest
    | (x,y) == ("turn", "off") = runRest (const False) rest
    | otherwise                = error "You messed up! FIX IT!"
    where
        runRest func [n1,_,n2] = modifyCell func (parseNumbers n1) (parseNumbers n2) rowSize

-- >>> :t map (parseToFunc . spaceSplit) testValues 
-- map (parseToFunc . spaceSplit) testValues :: [Bool -> Int -> Bool]

-- testing123 = runLightsChange testGrid (map (splitOn ' ') testValues)
-- testing123 = map (runLightsChange testGrid . splitOn ' ') testValues
-- testing234 = runLightsChange testGrid . splitOn ' ' $ "turn on 0,0 through 999,999"



-- >>> length testing234
-- 1000000

-- runAll lights [] = lights
-- runAll lights commands = 

-- testing223455 = foldl parseLineAndRun testGrid testValues
-- lbjwaoisfej = length $ filter (== True) testing223455

-- >>> lbjwaoisfej
-- 998996

parseToFuncs :: [[Char]] -> [Bool -> Int -> Bool]
parseToFuncs = map (parseToFunc . spaceSplit)

runOnCell :: [Bool -> Int -> Bool] -> (Bool,Int) -> Bool
runOnCell [] (cell,_) = cell
runOnCell (f:rest) (cell,pos) = runOnCell rest (f cell pos, pos)

-- testing552344 = runOnCell (parseToFuncs testValues) (False, 10)
-- >>> testing552344
-- True

calcAllCells :: [Bool -> Int -> Bool] -> [Bool]
calcAllCells funcs = [runOnCell funcs (False, x) | x <- [1 .. 1000000]]

parseToCells :: [[Char]] -> [Bool]
parseToCells = calcAllCells . parseToFuncs

runAll :: [[Char]] -> Int
runAll = length . filter (== True) . parseToCells

main = do
    contents <- readFile "input.txt"
    let result = runAll $ lines contents
    return result
-- answer: 543903