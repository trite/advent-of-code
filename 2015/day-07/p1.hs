import Data.Bits (Bits(shiftL, shiftR))
import Data.Map

test :: Int
test = shiftR 456 2

test2 :: Int
test2 = shiftL 123 2

-- >>> test
-- 114
-- >>> test2
-- 492

-- unsolved = fromList [('x',123), ('y',456), ('d', )]

