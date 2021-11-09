-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

test :: Int
test = shiftR 456 2

test2 :: Int
test2 = shiftL 123 2

-- >>> test
-- 114
-- >>> test2
-- 492

-- unsolved = fromList [('x',123), ('y',456), ('d', )]

-- data Wire val func1 func2 input =
--     Value val
--     | OneInput func1 input
--     | TwoInput func2 input input
--     deriving (Show)

-- solveWire wireMap (Value val) = Value val
-- solveWire wireMap (OneInput func x) =
--     maybe (OneInput func x) doRest (lookup x wireMap)
--     where
--         doRest (Value val) = Value $ func val
--         doRest other       = other -- Only solve if it was found
-- solveWire wireMap (TwoInput func x y) =
--     maybe (TwoInput func x y) nextLookup (lookup x wireMap)
--     where
--         nextLookup (Value rx) =
--             maybe (TwoInput func x y) doRest (lookup y wireMap)
--             where
--                 doRest (Value ry) = Value $ func rx ry
--         nextLookup other       = other

    -- case lookup x wireMap of
    --     Nothing   -> OneInput func x
    --     Just wire -> doRest wire
    --     where
    --         doRest (Value val) = Value $ func val
    --         doRest whatever    = whatever
-- solveWire wireMap (TwoInput func x y) =
--     case lookup x wireMap of
--         Nothing   -> TwoInput func x y
--         Just wire -> doRest wire
--         where
--             doRest 

-- >>> :info maybe
-- maybe :: b -> (a -> b) -> Maybe a -> b 	-- Defined in ‘Data.Maybe’

    -- -| Value val = Value val
    -- -| OneInput func1 in1 = 

-- type Unary  = Int -> Int
-- type Binary = Int -> Int -> Int
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

-- instance Applicative Wire where
--     pure = Value


testMap :: WireMap
testMap = fromList [
    ("x", Value 123),
    ("y", Value 456),
    ("d", NeedTwo (.&.) "x" "y"),
    ("e", NeedTwo (.|.) "x" "y"),
    ("f", NeedOne (`shiftL` 2) "x"),
    ("g", NeedOne (`shiftR` 2) "y"),
    ("h", NeedOne complement "x"),
    ("i", NeedOne complement "y")]

wireApply2 :: Wire -> Unary -> Maybe Wire -> Wire
wireApply2 _    func (Just (Value x)) = Value $ func x
wireApply2 wire _    _                = wire
-- wireApply2 _    func (Just wire)      = wire
-- wireApply2 wire func Nothing          = wire

wireApply3 :: Wire -> Binary -> Maybe Wire -> Maybe Wire -> Wire
wireApply3 _    func (Just (Value x)) (Just (Value y)) = Value $ func x y
wireApply3 wire _    _                _                = wire
-- wireApply3 func (Value x) (Value y) = Value $ func x y
-- wireApply3 func wireX     wireY     = NeedTwo func wireX wireY

solveWire :: WireMap -> Wire -> Wire
solveWire wireMap (Value val) = Value val -- already solved
solveWire wireMap (NeedOne func x) = wireApply2 (NeedOne func x) func (lookup x wireMap)
solveWire wireMap (NeedTwo func x y) = wireApply3 (NeedTwo func x y) func (lookup x wireMap) (lookup y wireMap)

isSolved :: Wire -> Bool
isSolved (Value _) = True
isSolved _         = False

getNext :: WireMap -> (WireMap, Int)
getNext wireMap = (nextMap, size $ Data.Map.filter isSolved nextMap)
    where
        nextMap = Data.Map.map (solveWire wireMap) wireMap

-- solveWireMap :: WireMap -> WireMap
solveWireMap :: WireMap -> WireMap
solveWireMap wireMap = recurse wireMap 0
    where
        recurse wireMap lastSize
            | nextSize == lastSize     = error "Circular dependency"
            | nextSize == size wireMap = nextWireMap
            | otherwise                = recurse nextWireMap nextSize
                where
                    (nextWireMap, nextSize) = getNext wireMap
                    -- nextWireMap = Data.Map.map (solveWire wireMap) wireMap
                    -- nextSize    = size (Data.Map.map (solveWire wireMap) wireMap) -- doing this twice isn't as efficient, 

-- >>> solveWireMap testMap
-- fromList [("d",72),("e",507),("f",492),("g",114),("h",65412),("i",65079),("x",123),("y",456)]

-- solveWireMap wireMap = Data.Map.map (solveWire wireMap) wireMap

-- solveWire wireMap (NeedOne func x) =
--     maybe
--         (NeedOne func x)    -- return same wire if Nothing
--         (wireApply2 func)    -- use wireApply to conditionally apply func
--         (lookup x wireMap)  -- apply it to the result of finding the wire name
-- solveWire wireMap (NeedTwo func x y) =



    -- maybe (NeedOne func x) (fmap func) (lookup x wireMap)

-- >>> :t fmap complement
-- fmap complement :: (Functor f, Bits b) => f b -> f b

-- map of values, 
-- solvePass wires =
--     map solve wire
--     where
--         solve (Value val) = Value val
--         solve (OneInput func1 in1)
--             | lookup 

-- >>> :info (&&)
-- (&&) :: Bool -> Bool -> Bool 	-- Defined in ‘GHC.Classes’
-- infixr 3 &&
-- >>> :info (||)
-- (||) :: Bool -> Bool -> Bool 	-- Defined in ‘GHC.Classes’
-- infixr 2 ||
-- >>> :info not
-- not :: Bool -> Bool 	-- Defined in ‘GHC.Classes’
-- >>> complement 5
-- -6
