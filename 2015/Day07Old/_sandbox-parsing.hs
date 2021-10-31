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

type Unary  = Word16 -> Word16
type Binary = Word16 -> Word16 -> Word16

type WireMap = Map String Wire

data Wire =
    Value Word16
    | NeedOne Unary String
    | NeedTwo Binary String String

instance Show Wire where
    show (Value x) = show $ concat ["(Value ", show x, ")"]
    show (NeedOne func x) = show $ concat ["(NeedOne ", x, ")"]
    show (NeedTwo func x y) = show $ concat ["(NeedTwo ", x, " ", y, ")"]

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
    | otherwise        = error "You done messed up now... FIX IT!"

parseWireParts3 :: [String] -> (String, Wire)
parseWireParts3 [val, _, name] = (name, Value $ read val)
parseWireParts3 _              = error "OH NOES"

parseWireParts4 :: [String] -> (String, Wire)
parseWireParts4 ["NOT", x, _, name] = (name, NeedOne complement x)
parseWireParts4 _                   = error "OH NOES"

parseWireParts5 :: [String] -> (String, Wire)
parseWireParts5 [x, "AND",    y, _, name] = (name, NeedTwo (.&.) x y)
parseWireParts5 [x, "OR",     y, _, name] = (name, NeedTwo (.|.) x y)
parseWireParts5 [x, "LSHIFT", y, _, name] = (name, NeedOne (`shiftL` read y) x)
parseWireParts5 [x, "RSHIFT", y, _, name] = (name, NeedOne (`shiftR` read y) x)
parseWireParts5 _ = error "OH NOES"


-- parseWire :: WireMap -> String -> WireMap
-- parseWire wireMap str = parseWireParts wireMap $ splitOn ' ' str

-- parseWireParts :: WireMap -> [String] -> WireMap
-- parseWireParts wireMap strs
--     | length strs == 3 = parseWireParts3 wireMap strs
--     | length strs == 4 = parseWireParts4 wireMap strs
--     | length strs == 5 = parseWireParts5 wireMap strs
--     | otherwise        = error "Ok, what did you goof up?"

-- parseWireParts3 :: WireMap -> [String] -> WireMap
-- parseWireParts3 wireMap [val, _, name] = insert name (Value (read val)) wireMap
-- parseWireParts3 _ _ = error "OH NOES"

-- parseWireParts4 :: WireMap -> [String] -> WireMap
-- parseWireParts4 wireMap ["NOT", x, _, name] = insert name (NeedOne complement x) wireMap
-- parseWireParts4 _ _ = error "OH NOES"

-- parseWireParts5 :: WireMap -> [String] -> WireMap
-- parseWireParts5 wireMap [x, "AND", y, _, name]    = insert name (NeedTwo (.&.) x y) wireMap
-- parseWireParts5 wireMap [x, "OR", y, _, name]     = insert name (NeedTwo (.|.) x y) wireMap
-- parseWireParts5 wireMap [x, "LSHIFT", y, _, name] = insert name (NeedOne (`shiftL` read y) x) wireMap
-- parseWireParts5 wireMap [x, "RSHIFT", y, _, name] = insert name (NeedOne (`shiftR` read y) x) wireMap
-- parseWireParts5 _ _ = error "OH NOES"




-- parseWireParts4 wireMap [x, "AND", y, _, name] = insert name ()

-- >>> parseWires toParse
-- fromList [
    -- ("d","(NeedTwo x y)"),
    -- ("e","(NeedTwo x y)"),
    -- ("f","(NeedOne x)"),
    -- ("g","(NeedOne y)"),
    -- ("h","(NeedOne x)"),
    -- ("i","(NeedOne y)"),
    -- ("x","(Value 123)"),
    -- ("y","(Value 456)")]
-- >>> length [1,2,3]
-- 3
