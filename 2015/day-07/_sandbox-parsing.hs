import Data.Map
import Data.Word (Word16)

type Unary  = Word16 -> Word16
type Binary = Word16 -> Word16 -> Word16

type WireMap = Map String Wire

data Wire =
    Value Word16
    | NeedOne Unary String
    | NeedTwo Binary String String

instance Show Wire where
    show (Value x) = show x
    show (NeedOne func x) = show $ concat ["(NeedOne ", x, ")"]
    show (NeedTwo func x y) = show $ concat ["(NeedTwo ", x, " ", y, ")"]