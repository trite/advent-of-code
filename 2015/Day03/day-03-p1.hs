import qualified Data.Map.Strict as M

-- moveCoord curCoords direction
moveCalc :: (Integer, Integer) -> Char -> (Integer, Integer)
moveCalc (x,y) '^' = (x+1,y)
moveCalc (x,y) 'v' = (x-1,y)
moveCalc (x,y) '>' = (x,y+1)
moveCalc (x,y) '<' = (x,y-1)
moveCalc _ _ = error "Something went wrong... FIX IT FIX IT FIX IT"

{-
>>> moveCalc (0,0) '^'
>>> moveCalc (0,0) 'v'
>>> moveCalc (0,0) '>'
>>> moveCalc (0,0) '<'
(1,0)
(-1,0)
(0,1)
(0,-1)
-}


moveTo :: (Ord k, Num a) => k -> M.Map k a -> M.Map k a
moveTo c = M.insertWith (+) c 1

testing = moveTo (0,0) $ M.fromList [((0,0), 2), ((0,1), 7)]
testing2 = moveTo (moveCalc (0,0) '^') (M.fromList [((0,0), 2), ((0,1), 7)])

-- >>> testing
-- fromList [((0,0),3),((0,1),7)]
-- >>> testing2
-- fromList [((0,0),2),((0,1),7),((1,0),1)]




loop' :: M.Map (Integer, Integer) Integer -- ^ Move list
    -> (Integer, Integer) -- ^ Current position
    -> [Char] -- ^ Remaining moves
    -> M.Map (Integer, Integer) Integer
loop' prevMoves _ [] = prevMoves
loop' prevMoves pos (x:rest) = loop' (moveTo newPos prevMoves) newPos rest
    where newPos = moveCalc pos x

testing3 = loop' (M.fromList [((0,0), 1)]) (0,0) ">>^v<<"
-- >>> testing3
-- fromList [((0,0),2),((0,1),2),((0,2),2),((1,2),1)]

loop = loop' (M.fromList [((0,0), 1)]) (0,0)
testing4 = length (loop ">v>^<v<")
-- >>> testing4
-- 6

eval str = length (loop str)
{-
>>> eval ">"
>>> eval "^>v<"
>>> eval "^v^v^v^v^v"
2
4
2
-}

main = do
    contents <- readFile "day-03-input.txt"
    let result = eval contents
    return result

{-
PS C:\Git\advent-of-code\2015> ghci .\day-03-p1.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( day-03-p1.hs, interpreted )
Ok, one module loaded.
*Main> main
2081
-}