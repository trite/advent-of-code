{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- threeVowels str = filter 
{-
>>> 'q' `elem` ['a','e','i','o','u']
False
>>> 'a' `elem` ['a','e','i','o','u']
True
-}
vowels :: [Char] -> Bool
vowels str = length (filter (`elem` ['a','e','i','o','u']) str) >= 3
{-
>>> vowels "aei"
True
>>> map vowels ["aei", "xazegov", "aeiouaeiouaeiou", "nlwp2390i4", "a", "b", "abc"]
[True,True,True,False,False,False,False]
-}

doubleLetter :: [Char] -> Bool
doubleLetter (x:str) = recurse x str
    where
        recurse :: Char -> [Char] -> Bool
        recurse _ [] = False
        recurse p (c:rest)
            | p == c    = True
            | otherwise = recurse c rest
{-
>>> doubleLetter "abcdde"
True
>>> map doubleLetter ["abcdde", "aabbccdd", "aa", "bbb", "abcdefgh", "a", "12354asdb", "abc1", "1"]
[True,True,True,True,False,False,False,False,False]
-}

dealBreaker :: (Char, Char) -> Bool
dealBreaker pair = pair `elem` [('a','b'), ('c','d'), ('p','q'), ('x','y')]
{-
>>> dealBreaker ('q','x')
False
>>> map dealBreaker [('a','b'), ('x','y'), ('E','T'), ('x','7'), ('o','k')]
[True,True,False,False,False]
-}

-- TODO: Figure out if there's a built-in fold method to abstract out dealBreakers and doubleLetter
dealBreakers :: [Char] -> Bool
dealBreakers (x:str) = recurse x str
    where
        recurse :: Char -> [Char] -> Bool
        recurse _ [] = True
        recurse p (c:rest)
            | dealBreaker (p, c) = False
            | otherwise          = recurse c rest
{-
>>> dealBreakers "irowperiunwer"
True
>>> map dealBreakers ["ertyuiop", "asdf", "tribbles", "asdfabcdpqxy", "xy", "abcd", "acbd"]
[True,True,True,False,False,False,True]
-}

valid :: [Char] -> Bool
valid str = all ($ str) [vowels, doubleLetter, dealBreakers]
{-
>>> map valid ["ugknbfddgicrmopn","jchzalrnumimnmhp","haegwjzuvuyypxyu","dvszwmarrgswjxmb"]
[True,False,False,False]
-}

main = do
    contents <- readFile "input.txt"
    let result = length (filter valid (lines contents))
    return result
-- Answer: 258