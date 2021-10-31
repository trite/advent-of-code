{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative hiding (some)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

myParser :: Parser String
myParser = some (char 'a')

test = parse myParser "" "aaa"
-- >>> test
-- Right "aaa"

main :: IO ()
main = undefined
-- main = hspec $
--   describe "myParser" $ do
--     it "returns correct result" $
--       parse myParser "" "aaa" `shouldParse` "aaa"
--     it "result of parsing satisfies what it should" $
--       parse myParser "" "aaaa" `parseSatisfies` ((== 4) . length)
