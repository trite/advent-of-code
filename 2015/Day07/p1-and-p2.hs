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
import Text.Read (readMaybe)

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

-- Wire logic
wirePending :: Wire -> Maybe Wire -> Wire
wirePending _    (Just (Value x)) = Value x
wirePending wire _                = wire

wireApply2 :: Wire -> Unary -> Maybe Wire -> Wire
wireApply2 _    func (Just (Value x)) = Value $ func x
wireApply2 wire _    _                = wire

wireApply3 :: Wire -> Binary -> Maybe Wire -> Maybe Wire -> Wire
wireApply3 _    func (Just (Value x)) (Just (Value y)) = Value $ func x y
wireApply3 wire _    _                _                = wire

solveWire :: WireMap -> Wire -> Wire
solveWire wireMap (Value val) = Value val
solveWire wireMap (Pending x) = wirePending (Pending x) (lookup x wireMap)
solveWire wireMap (NeedOne func x) = wireApply2 (NeedOne func x) func (lookup x wireMap)
-- For dealing with entries such as "1 AND gd -> ge"
solveWire wireMap (NeedTwo func "1" y) = wireApply3 (NeedTwo func "1" y) func (Just $ Value 1) (lookup y wireMap)
solveWire wireMap (NeedTwo func x y) = wireApply3 (NeedTwo func x y) func (lookup x wireMap) (lookup y wireMap)

isUnsolved :: Wire -> Bool
isUnsolved (Value _) = False
isUnsolved _         = True

unsolvedCount wireMap = size $ Data.Map.filter isUnsolved wireMap

getNext :: WireMap -> (WireMap, Int)
getNext wireMap = (nextMap, unsolvedCount nextMap)
    where
        nextMap = Data.Map.map (solveWire wireMap) wireMap

solveWireMap :: WireMap -> WireMap
solveWireMap wireMap = recurse wireMap 0
    where
        recurse wireMap lastSize
            -- -| nextSize == lastSize = error $ concat ["Circular reference. nextSize: [", show nextSize, "] lastSize: [", show lastSize, "]"]
            | nextSize == lastSize = error $ show wireMap
            | nextSize == 0        = nextWireMap
            | otherwise            = recurse nextWireMap nextSize
                where
                    (nextWireMap, nextSize) = getNext wireMap

{-
>>> testMap = fromList [ ("x", Value 123), ("y", Value 456), ("d", NeedTwo (.&.) "x" "y"), ("e", NeedTwo (.|.) "x" "y"), ("f", NeedOne (`shiftL` 2) "x"), ("g", NeedOne (`shiftR` 2) "y"), ("h", NeedOne complement "x"), ("i", NeedOne complement "y")]
>>> solveWireMap testMap
fromList [("d",72),("e",507),("f",492),("g",114),("h",65412),("i",65079),("x",123),("y",456)]
-}


-- Parsing logic

splitOn :: Char -> String -> [String]
splitOn = splitOn' [] []
    where
        splitOn' :: [String] -> String -> Char -> String -> [String]
        splitOn' accList acc _ [] = reverse $ reverse acc:accList
        splitOn' accList acc split (x:rest)
            | split == x = splitOn' (reverse acc:accList) [] split rest
            | otherwise  = splitOn' accList (x:acc) split rest

parseWires :: [String] -> WireMap
parseWires = recurse empty
    where
        recurse wireMap [] = wireMap
        recurse wireMap (x:xs) = recurse (parseWire wireMap x) xs

parseWire :: WireMap -> String -> WireMap
parseWire wireMap str = insert name wire wireMap
    where
        (name, wire) = parseWireParts $ splitOn ' ' str

parseWireParts :: [String] -> (String, Wire)
parseWireParts strs
    | length strs == 3 = parseWireParts3 strs
    | length strs == 4 = parseWireParts4 strs
    | length strs == 5 = parseWireParts5 strs
    | otherwise        = error "Unexpected number of arguments"

parseWireParts3 :: [String] -> (String, Wire)
parseWireParts3 [val, _, name] =
    case readMaybe val of
        Just wordVal -> (name, Value wordVal)
        Nothing      -> (name, Pending val)
    -- maybe (name, Pending val) (\wordVal -> (name, Value wordVal)) (readMaybe val) 
parseWireParts3 _              = error "Called parseWireParts3 with the wrong number of arguments?"

parseWireParts4 :: [String] -> (String, Wire)
parseWireParts4 ["NOT", x, _, name] = (name, NeedOne complement x)
parseWireParts4 _                   = error "Called parseWireParts4 with the wrong number of arguments?"

parseWireParts5 :: [String] -> (String, Wire)
parseWireParts5 [x, "AND",    y, _, name] = (name, NeedTwo (.&.) x y)
parseWireParts5 [x, "OR",     y, _, name] = (name, NeedTwo (.|.) x y)
parseWireParts5 [x, "LSHIFT", y, _, name] = (name, NeedOne (`shiftL` read y) x)
parseWireParts5 [x, "RSHIFT", y, _, name] = (name, NeedOne (`shiftR` read y) x)
parseWireParts5 _ = error "Called parseWireParts5 with the wrong number of arguments?"

-- Sanity check
toParse :: [String]
toParse = [
    "123 -> x",
    "456 -> y",
    "x AND y -> d",
    "x OR y -> e",
    "x LSHIFT 2 -> f",
    "y RSHIFT 2 -> g",
    "NOT x -> h",
    "NOT y -> i"]

-- >>> solveWireMap $ parseWires toParse
-- fromList [("d",72),("e",507),("f",492),("g",114),("h",65412),("i",65079),("x",123),("y",456)]




-- Solving part 1:

-- main = do
--     contents <- readFile "input.txt"
--     let solvedMap = solveWireMap $ parseWires $ lines contents
--     let result = lookup "a" solvedMap
--     return result

-- Result: 46065




-- Solving part 2:

main = do
    contents <- readFile "input_p2.txt"
    let solvedMap = solveWireMap $ parseWires $ lines contents
    let result = lookup "a" solvedMap
    return result

-- Result: 14134