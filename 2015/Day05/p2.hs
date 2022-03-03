pairOff [] = error "oh noes!"
pairOff (x:rest) = recurse [] x rest
    where
        recurse acc _ [] = acc
        recurse acc x (y:rest) = recurse ((x,y):acc) y rest
-- >>> pairOff "blargh"
-- [('g','h'),('r','g'),('a','r'),('l','a'),('b','l')]

-- checkPairs (x:rest) = recurse x rest
--     where
--         recurse _ [] = False
--         recurse 

checkPairs :: Eq a => [a] -> Bool
checkPairs (x:y:rest)
    | x `elem` rest = True
    | otherwise     = checkPairs (y:rest)
checkPairs (x:rest) = False
checkPairs [] = False

twoPairs :: Eq t => [t] -> Bool
twoPairs lst = checkPairs $ pairOff lst
{-
>>> map twoPairs ["xyxy", "aabcdefgaa", "aaa"]
[True,True,False]
-}

triplets :: Eq a => [a] -> Bool
triplets (x:y:z:rest)
    | x == z    = True
    | otherwise = triplets (y:z:rest)
triplets _ = False

{-
>>> map triplets ["xyx", "abcdefeghi", "aaa", "abc"]
[True,True,True,False]
-}

valid :: Eq t => [t] -> Bool
valid str = all ($ str) [twoPairs, triplets]

{-
>>> map valid ["qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy"]
[True,True,False,False]
-}

main = do
    contents <- readFile "input.txt"
    let result = length (filter valid (lines contents))
    return result
-- Answer: 53