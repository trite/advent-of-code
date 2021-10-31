module Wire
    ( Unary
    , Binary
    , WireMap
    , Wire
    ) where

type Unary  = Word16 -> Word16
type Binary = Word16 -> Word16 -> Word16

type WireMap = Map String Wire

data Wire =
    Value Word16
    | Pending String
    | NeedOne Unary String
    | NeedTwo Binary String String

instance Show Wire where
    show (Value x) = show $ concat ["(Value ", show x, ")"]
    show (Pending x) = show $ concat ["(Pending ", x, ")"]
    show (NeedOne func x) = show $ concat ["(NeedOne ", x, ")"]
    show (NeedTwo func x y) = show $ concat ["(NeedTwo ", x, " ", y, ")"]