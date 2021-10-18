createGrid x y = [ False | x <- [1..x], y <- [1..y] ]

createSquareGrid x = createGrid x x

modifySquare :: (Bool -> Bool) -> Int -> [Bool] -> (Int,Int) -> (Int,Int) -> [Bool]
modifySquare func rowSize list (sx,sy) (ex,ey)
    | sx <= ex && sy <= ey = recurse [] func rowSize list (sx,sy) (ex,ey) 0
    | otherwise            = error "End x and y values must be greater than starting x and y values"
    where
        recurse :: [Bool] -> (Bool -> Bool) -> Int -> [Bool] -> (Int,Int) -> (Int,Int) -> Int -> [Bool]
        recurse acc _ _ [] _ _ _ = reverse acc
        recurse acc func rowSize (next:rest) start end pos
            | isInRange rowSize start end pos = recurse (func next:acc) func rowSize rest start end (pos+1)
            | otherwise                       = recurse (next:acc) func rowSize rest start end (pos+1)
            where
                isInRange :: Int -> (Int,Int) -> (Int,Int) -> Int -> Bool
                isInRange rowSize (sx,sy) (ex,ey) pos = sx <= (pos `div` rowSize) &&
                                                        (pos `div` rowSize) <= ex &&
                                                        sy <= (pos `mod` rowSize) &&
                                                        (pos `mod` rowSize) <= ey

-- type LightChange :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]

turnOff :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
turnOff = modifySquare (const False) rowSize

turnOn :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
turnOn = modifySquare (const True) rowSize

toggle :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
toggle = modifySquare not rowSize








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

runLightsChange list (x:y:rest)
    | x == "toggle"            = runRest toggle list (y:rest)
    | (x,y) == ("turn", "on")  = runRest turnOn list rest
    | (x,y) == ("turn", "off") = runRest turnOff list rest
    | otherwise                = error "This shouldn't happen, you done messed up!"

-- >>> runLightsChange [False,False,False,False] ["turn", "on", "0,0", "through", "0,1"]
-- [True,True,False,False]

runRest func list [n1,_,n2] = func list (parseNumbers n1) (parseNumbers n2)



rowSize = 1000

testGrid = createSquareGrid rowSize

testValues = ["turn on 0,0 through 999,999", "toggle 0,0 through 999,0", "turn off 499,499 through 500,500"]

-- testing123 = runLightsChange testGrid (map (splitOn ' ') testValues)
testing123 = map (runLightsChange testGrid . splitOn ' ') testValues
testing234 = runLightsChange testGrid . splitOn ' ' $ "turn on 0,0 through 999,999"

parseLineAndRun :: [Bool] -> [Char] -> [Bool]
parseLineAndRun lights = runLightsChange lights . splitOn ' '

runAll cmds = length $ filter (== True) $ foldl parseLineAndRun testGrid cmds

-- >>> length testing234
-- 1000000

-- runAll lights [] = lights
-- runAll lights commands = 

testing223455 = foldl parseLineAndRun testGrid testValues
lbjwaoisfej = length $ filter (== True) testing223455

-- >>> lbjwaoisfej
-- 998996

main = do
    contents <- readFile "input.txt"
    let result = runAll $ lines contents
    return result