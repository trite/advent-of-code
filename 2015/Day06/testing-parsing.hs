-- {-# LANGUAGE OverloadedStrings #-}

-- import Control.Applicative hiding (some)
-- import Data.Text (Text)
-- import Data.Void
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- type Parser = Parsec Void Text

-- myParser :: Parser String
-- myParser = some (char 'a')

-- test = parse myParser "" "aaaaaaaa"
-- -- >>> test
-- -- Right "aaaaaaaa"


testValues = ["turn on 0,0 through 999,999", "toggle 0,0 through 999,0", "turn off 499,499 through 500,500"]



splitOn :: Char -> String -> [String]
splitOn = splitOn' [] []
    where
        splitOn' :: [String] -- ^ accList - previous accumulations
            -> String        -- ^ acc - current accumulator
            -> Char          -- ^ split - character to split on
            -> String        -- ^ rest of the string
            -> [String]
        splitOn' accList acc _ [] = reverse $ reverse acc:accList
        splitOn' accList acc split (x:rest)
            | split == x = splitOn' (reverse acc:accList) [] split rest
            | otherwise  = splitOn' accList (x:acc) split rest

-- >>> map (splitOn ' ') testValues
-- [["turn","on","0,0","through","999,999"],["toggle","0,0","through","999,0"],["turn","off","499,499","through","500,500"]]

actionDecision (x:y:rest)
    | x == "toggle" = "it was toggle"
    | (x,y) == ("turn", "on") = "it was turn on"
    | (x,y) == ("turn", "off") = "it was turn off"
    | otherwise             = "something else"

temp01 = map (splitOn ' ') testValues
temp02 = map actionDecision temp01

-- >>> temp02
-- ["it was turn on","it was toggle","it was turn off"]


