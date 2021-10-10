basement' :: Int -- ^ Current character counter
  -> Int -- ^ Current floor number
  -> String -- ^ Rest of the characters
  -> Int
basement' ct flr (x:rest)
    | flr == -1 = ct
    | otherwise = basement' (ct+1) (check flr x) rest
    where
        check flr '(' = flr + 1
        check flr ')' = flr - 1
        check flr x = error "uh oh 1"
basement' ct _ [] = ct

basement :: String -> Int
basement = basement' 0 0

-- >>> basement ")"
-- 1
-- >>> basement "()())"
-- 5

main = do
    contents <- readFile "day-01-input.txt"
    let result = basement contents
    return result

{-
PS C:\git\AdventOfCode\2015> ghci .\day-02.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( day-02.hs, interpreted )
Ok, one module loaded.
*Main> main
1783
-}