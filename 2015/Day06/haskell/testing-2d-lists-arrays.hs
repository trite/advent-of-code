import Data.Array.Unboxed (UArray, listArray, (!))

grid = listArray ((0,0), (9,9)) (replicate 100 False) :: UArray (Int,Int) Bool

{- In a 1000 x 1000 grid doesn't the first 3 digits basically indicate the y and the second 3 the x?
Cell (27,42) would be found at the 027042 position in the list I think... -}

gridList = [ x * 1000 + y == 27042 | x <- [0..999], y <- [0..999] ]
gridArr = listArray ((0,0), (999,999)) gridList :: UArray (Int,Int) Bool

-- >>> gridArr ! (27,42)
-- True

createGrid x y = [ False | x <- [1..x], y <- [1..y] ]

createSquareGrid x = createGrid x x

smallList = createGrid 4 4
-- >>> smallList
-- [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]

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

{-
>>> 1 `div` 1
>>> 1 `mod` 1
1
0
-}

turnOff :: Int -> [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
turnOff = modifySquare (const False)

turnOn :: Int -> [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
turnOn = modifySquare (const True)

toggle :: Int -> [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
toggle = modifySquare not

turnOffSquare :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
turnOffSquare = turnOff rowSize

turnOnSquare :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
turnOnSquare = turnOn rowSize

toggleSquare :: [Bool] -> (Int, Int) -> (Int, Int) -> [Bool]
toggleSquare = toggle rowSize


-- join :: String -> Char -> String
-- join (str:rest) c = recurse str rest c
--     where
--         recurse :: [String] -> [String] -> Char -> String
--         recurse acc []         _ = acc
--         recurse acc (str:rest) c = recurse (acc ++ [c] ++ show str) rest c

join' :: (Show a) => String -> String -> a -> String
join' str split toJoin = str ++ split ++ show toJoin


join :: (Show a) => [a] -> String -> String
join []       []      = error "why?!"
join []       (_:_)   = error "also why?!"
join (x:rest) between = recurse (show x) rest between
    where
        recurse :: (Show a) => String -> [a] -> String -> String
        recurse acc []       _       = acc
        recurse acc (x:rest) between = recurse (acc ++ between ++ show x) rest between

-- testing = join (toggleSquare testGrid (1,1) (2,2)) "_"

formatGrid []   _       = error "stop that!"
formatGrid list rowSize = recurse "" list rowSize
    where
        recurse acc []   _       = acc
        recurse []  list rowSize = recurse newHead newList rowSize
            where
                newHead = join (take rowSize list) ","
                newList = drop rowSize list
        recurse acc list rowSize = recurse (acc ++ "\n" ++ newHead) newList rowSize
            where
                newHead = join (take rowSize list) ","
                newList = drop rowSize list
{-
>>> formatGrid (toggleSquare testGrid (1,1) (2,2)) rowSize
"False,False,False,False
False,True,True,False
False,True,True,False
False,False,False,False"
-}


rowSize = 4

testGrid = createSquareGrid rowSize

