import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)

-- |The 'hash' takes a string and returns the hexadecimal representation of its MD5 checksum
hash :: String -> String
hash = unpack . encode . MD5.hash . pack