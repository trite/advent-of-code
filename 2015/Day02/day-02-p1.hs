splitOn' :: [String] -- ^ accList - previous accumulations
  -> String -- ^ acc - current accumulator
  -> Char -- ^ split - character to split on
  -> String -- ^ rest
  -> [String]
splitOn' accList acc _ [] = reverse $ reverse acc:accList
splitOn' accList acc split (x:rest)
    | split == x = splitOn' (reverse acc:accList) [] split rest
    | otherwise  = splitOn' accList (x:acc) split rest

splitOn :: Char -> String -> [String]
splitOn = splitOn' [] []

-- >>> splitOn 'x' "123x2x234"
-- ["123","2","234"]

intify :: String -> Int
intify x = read x :: Int

-- >>> intify "23"
-- 23
-- >>> map intify $ splitOn 'x' "123x2x234"
-- [123,2,234]

area :: (Num p, Ord p) => [p] -> p
area [w, l, h] = 2*w*l + 2*w*h + 2*l*h + slack w l h
    where slack w l h = minimum [w*l, w*h, l*h]
area other = error "You're doing it wrong!"

-- >>> area $ map intify $ splitOn 'x' "2x3x4"
-- 58

calc :: String -> Int
calc str = area $ map intify $ splitOn 'x' str

-- >>> calc "2x3x4"
-- 58
-- >>> calc "1x1x10"
-- 43

main = do
    contents <- readFile "day-02-input.txt"
    let result = sum $ map calc $ lines contents
    return result

{-
PS C:\Git\advent-of-code\2015> ghci .\day-02-p1.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( day-02-p1.hs, interpreted )
Ok, one module loaded.
*Main> main
1606483
-}




