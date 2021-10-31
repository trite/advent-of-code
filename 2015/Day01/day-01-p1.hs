import Data.Map

testing :: Map [Char] Int
testing = fromList [ ("(())", 0)
                   , ("()()", 0)
                   , ("(((", 3)
                   , ("(()(()(", 3)
                   , ("))(((((", 3)
                   , ("())", -1)
                   , ("))(", -1)
                   , (")))", -3)
                   , (")())())", -3) ]

charCount :: Char -> String -> Int
charCount chr str = length (Prelude.filter (== chr) str)

lParen :: String -> Int
lParen = charCount '('

rParen :: String -> Int
rParen = charCount ')'

floorNum :: String -> Int
floorNum str = lParen str - rParen str

failures :: Map String Int -> Map String Int
failures = filterWithKey (\ k v -> v /= floorNum k)

-- >>> failures testing
-- fromList []

main = do
    contents <- readFile "day-01-input.txt"
    let result = floorNum contents
    return result

{-
PS C:\git\AdventOfCode\2015> ghci .\day-01.hs       
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( day-01.hs, interpreted )
Ok, one module loaded.
*Main> main
232
-}
