import Data.Array.Unboxed (UArray, listArray, (!))

grid = listArray ((0,0), (9,9)) (replicate 100 False) :: UArray (Int,Int) Bool

{- In a 1000 x 1000 grid doesn't the first 3 digits basically indicate the y and the second 3 the x?
Cell (27,42) would be found at the 027042 position in the list I think... -}

gridList = [ x * 1000 + y == 27042 | x <- [0..999], y <- [0..999] ]
gridArr = listArray ((0,0), (999,999)) gridList :: UArray (Int,Int) Bool

-- >>> gridArr ! (27,42)
-- True

createGrid x y = [ False | x <- [1..x], y <- [1..y] ]

smallList = createGrid 4 4
-- >>> smallList
-- [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]

-- modifySquare :: (Bool -> Bool) -> [Bool] -> (Int,Int) -> (Int,Int) -> [Bool]
-- modifySquare func list rowSize (sx,sy) (ex,ey) = 

-- >>> 