{-# LANGUAGE FlexibleContexts #-} -- need to figure out why this is needed for solveWireMap to work right
import Data.Bits (
    Bits(
        shiftL,
        shiftR,
        complement,
        (.&.),
        (.|.)
    ))
import Data.Map
import Data.Word (Word16)
import Data.List (concat)
import Prelude hiding (lookup)


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


wireApply2 :: Wire -> Unary -> Maybe Wire -> Wire
wireApply2 _    func (Just (Value x)) = Value $ func x
wireApply2 wire _    _                = wire

wireApply3 :: Wire -> Binary -> Maybe Wire -> Maybe Wire -> Wire
wireApply3 _    func (Just (Value x)) (Just (Value y)) = Value $ func x y
wireApply3 wire _    _                _                = wire

solveWire :: WireMap -> Wire -> Wire
solveWire wireMap (Value val) = Value val
solveWire wireMap (NeedOne func x) = wireApply2 (NeedOne func x) func (lookup x wireMap)
solveWire wireMap (NeedTwo func x y) = wireApply3 (NeedTwo func x y) func (lookup x wireMap) (lookup y wireMap)

isSolved :: Wire -> Bool
isSolved (Value _) = True
isSolved _         = False

getNext :: WireMap -> (WireMap, Int)
getNext wireMap = (nextMap, size nextMap)
    where
        nextMap = Data.Map.map (solveWire wireMap) wireMap

solveWireMap :: WireMap -> WireMap
solveWireMap wireMap = recurse wireMap 0
    where
        recurse wireMap lastSize
            | nextSize == lastSize     = error "Circular dependency"
            | nextSize == size wireMap = nextWireMap
            | otherwise                = recurse nextWireMap nextSize
                where
                    (nextWireMap, nextSize) = getNext wireMap

{-
>>> testMap = fromList [ ("x", Value 123), ("y", Value 456), ("d", NeedTwo (.&.) "x" "y"), ("e", NeedTwo (.|.) "x" "y"), ("f", NeedOne (`shiftL` 2) "x"), ("g", NeedOne (`shiftR` 2) "y"), ("h", NeedOne complement "x"), ("i", NeedOne complement "y")]
>>> solveWireMap testMap
fromList [("d",72),("e",507),("f",492),("g",114),("h",65412),("i",65079),("x",123),("y",456)]
-}