{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Data.Map.Strict as M

moveCalc :: (Integer, Integer) -> Char -> (Integer, Integer)
moveCalc (x,y) '^' = (x+1,y)
moveCalc (x,y) 'v' = (x-1,y)
moveCalc (x,y) '>' = (x,y+1)
moveCalc (x,y) '<' = (x,y-1)
moveCalc _ _ = error "Something went wrong... FIX IT FIX IT FIX IT"


moveTo :: (Ord k, Num a) => k -> M.Map k a -> M.Map k a
moveTo c = M.insertWith (+) c 1


-- loop' :: M.Map (Integer, Integer) Integer -- ^ Move list
--     -> (Integer, Integer) -- ^ Current position
--     -> [Char] -- ^ Remaining moves
--     -> M.Map (Integer, Integer) Integer
-- loop' prevMoves _ [] = prevMoves
-- loop' prevMoves pos (x:rest) = loop' (moveTo newPos prevMoves) newPos rest
--     where newPos = moveCalc pos x

loop' :: M.Map (Integer, Integer) Integer -- ^ Move list
    -> (Integer, Integer) -- ^ Santa position
    -> (Integer, Integer) -- ^ Robo-Santa position
    -> String -- ^ Remaining moves
    -> M.Map (Integer, Integer) Integer
loop' ml _ _ [] = ml
loop' ml sp rp (s:r:rest) = loop' (moveTo nsp . moveTo nrp $ ml) nsp nrp rest
    where nsp = moveCalc sp s
          nrp = moveCalc rp r

-- loop = loop' (M.fromList [((0,0), 1)]) (0,0)
loop = loop' (M.fromList [((0,0), 1)]) (0,0) (0,0)

-- >>> loop "^v"
-- fromList [((-1,0),1),((0,0),1),((1,0),1)]

eval str = length (loop str)

{-
>>> eval "^v"
>>> eval "^>v<"
>>> eval "^v^v^v^v^v"
3
3
11
-}

main = do
    contents <- readFile "day-03-input.txt"
    let result = eval contents
    return result

{-
PS C:\Git\advent-of-code\2015> ghci .\day-03-p2.hs 
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( day-03-p2.hs, interpreted )
Ok, one module loaded.
*Main> main
2341
-}