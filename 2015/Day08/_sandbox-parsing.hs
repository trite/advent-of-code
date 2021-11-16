-- Checking out a few examples of parsers from scratch to help get more familiar
-- Functor/applicative/monad/etc bits from: https://www.youtube.com/watch?v=LeoDCiu_GB0

import Data.Char
import Data.List

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s -> do
        (x', s') <- x s
        return (f x', s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)

    (Parser f) <*> (Parser x) = Parser $ \s -> do
        (f', s1) <- f s
        (x', s2) <- x s1
        return (f' x', s2)

instance Monad Parser where
    (Parser x) >>= f = Parser $ \s -> do
        (x', s') <- x s
        runParser (f x') s'

instance MonadFail Parser where
    fail _ = Parser $ const Nothing

class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

    some  :: f a -> f [a]
    some v = some_v
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v

    many  :: f a -> f [a]
    many v = many_v
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v

instance Alternative Parser where
    empty = fail ""
    (Parser x) <|> (Parser y) = Parser $ \s ->
        case x s of
            Just x  -> Just x
            Nothing -> y s

char :: Char -> Parser Char
char c = Parser charP
    where charP [] = Nothing
          charP (x:xs)
            | x == c    = Just (c,xs)
            | otherwise = Nothing
-- >>> parseD = char 'd' <|> char 'D'
-- >>> runParser parseD "D"
-- Just ('D',"")

string :: String -> Parser String
string = mapM char
-- >>> runParser (string "Paul") "Paulblargh"
-- >>> p = (,) <$> string "Paul" <*> string "blargh"
-- >>> runParser p "Paulblargh"
-- Just ("Paul","blargh")
-- Just (("Paul","blargh"),"")

space :: Parser Char
space =
    char ' '
    <|> char '\n'
    <|> char '\r'
    <|> char '\t'

ss :: Parser String
ss = many space

-- parseInt'

quote :: Parser Char
quote = char '"'

-- combine' :: [Parser Char] -> Parser Char -> Parser Char
-- combine' []     acc = acc
-- combine' (x:xs) acc = combine' xs (x <|> acc)

combine :: [Parser Char] -> Parser Char
-- combine (x:xs) = combine' xs x
combine (x:xs) = foldl (flip (<|>)) x xs
combine []     = error "This shouldn't happen"

-- letters :: [Parser Char]
testLetters = combine [char 'a', char 'b']

testLetters2 = combine $ char . chr <$> [65..66]

-- >>> runParser testLetters "a"
-- >>> runParser testLetters "b"
-- >>> runParser testLetters "c"
-- Just ('a',"")
-- Just ('b',"")
-- Nothing

-- >>> runParser testLetters2 "A"
-- >>> runParser testLetters2 "B"
-- >>> runParser testLetters2 "C"
-- Just ('A',"")
-- Just ('B',"")
-- Nothing

-- >>> ord <$> ['a','z','A','Z']
-- [97,122,65,90]

-- >>> chr <$> [65..90] ++ [97..122]
-- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

letter :: Parser Char
letter = combine $ char . chr <$> ([65..90] ++ [97..122])

-- valid :: Parser String
-- valid = letter <|> string "\\\\"

-- letters = map letter

-- >>> runParser letter "abcDEF"
-- Just ('a',"bcDEF")

-- >>> [65..90] ++ [97..122]
-- [65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122]

-- letter :: Parser Char
-- letter = 

parserHW = (,) <$> (string "Hello" <* ss) <*> string "World"
-- >>> runParser parserHW "Hello        World"
-- Just (("Hello","World"),"")

-- >>> ord 'z'
-- 122

-- >>> chr <$> [65..67]
-- "ABC"

-- >>> map chr [65..67]
-- "ABC"
