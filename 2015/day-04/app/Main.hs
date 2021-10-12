-- TODO: Still can't figure out why this builds/runs fine but the VSCode Haskell tools don't like it
module Main where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)

-- |The 'hash' takes a string and returns the hexadecimal representation of its MD5 checksum
hash :: String -> String
hash = unpack . encode . MD5.hash . pack

fiveZeros :: String {- Secret Key -} -> Integer
fiveZeros sKey = recurse sKey 1
    where
        recurse :: String {-sKey-} -> Integer {-to test-} -> Integer
        recurse sKey test
            | hash (sKey ++ show test) `startsWith` "000000" = test
            | otherwise = recurse sKey (test+1)

startsWith :: Eq a => [a] -> [a] -> Bool
suffix `startsWith` prefix
    | length prefix <= length suffix = not (any (uncurry (/=)) (zip prefix suffix))
    | otherwise                      = False

-- testing = takeWhile (\x -> ) [1..]

main :: IO ()
-- main = print (fiveZeros "abcdef")
-- ^-- result was correct: 609043
main = print (fiveZeros "ckczppom")
-- ^-- result: 117946 (correct)
-- answer with 6 zeros: 3938038 (correct)