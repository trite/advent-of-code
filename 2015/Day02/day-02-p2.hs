splitOn' :: [String] -- ^ accList - previous accumulations
  -> [Char] -- ^ acc - current accumulator
  -> Char -- ^ split - character to split on
  -> [Char] -- ^ rest
  -> [String]
splitOn' accList acc _ [] = reverse $ reverse acc:accList
splitOn' accList acc split (x:rest)
    | split == x = splitOn' (reverse acc:accList) [] split rest
    | otherwise  = splitOn' accList (x:acc) split rest

splitOn :: Char -> [Char] -> [String]
splitOn = splitOn' [] []

intify :: String -> Int
intify x = read x :: Int

parse str = map intify $ splitOn 'x' str

ribLen :: (Num p, Ord p) => [p] -> p
ribLen [w,l,h] = w*l*h + minPerim w l h
    where minPerim w l h = minimum [2*w + 2*l, 2*w + 2*h, 2*l + 2*h]
ribLen other = error "This shouldn't happen!"

-- >>> ribLen [2,3,4]
-- 34
-- >>> ribLen [1,1,10]
-- 14


main = do
    contents <- readFile "day-02-input.txt"
    let result = sum $ map (ribLen . parse) (lines contents)
    return result

{-
PS C:\Git\advent-of-code\2015> ghci .\day-02-p2.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/    :? for help
[1 of 1] Compiling Main                         ( day-02-p2.hs, interpreted )
Ok, one module loaded.
*Main> main
3842356
-}