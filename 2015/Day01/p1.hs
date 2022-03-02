import Data.Map hiding (filter)
{-# ANN module "HLint: ignore Eta reduce" #-}

testing :: Map String Int
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
charCount chr str = length $ filter (== chr) str

lParen :: String -> Int
lParen str = charCount '(' str

rParen :: String -> Int
rParen str = charCount ')' str

floorNum :: String -> Int
floorNum str = lParen str - rParen str

failures :: Map String Int -> Map String Int
failures lst = filterWithKey (\ k v -> v /= floorNum k) lst

-- >>> failures testing
-- fromList []

main = do
    contents <- readFile "input.txt"
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
