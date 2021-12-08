{-# LANGUAGE FlexibleContexts #-}
{-# ANN module "HLint: ignore Eta reduce" #-}

trip (x:y:rest) = recurse [] x y rest
    where
        recurse acc _ _ [] = reverse acc
        recurse acc x y (z:rest) = recurse (x+y+z:acc) y z rest
trip _ = error "Trip error"
-- >>> trip testList
-- [607,618,618,617,647,716,769,792]

count list =
    length $ filter (\(x,y) -> y > x) list
-- >>> count paired
-- 7

pair [] = error "pairror"
pair (x:xs) = recurse [] x xs
    where
        recurse acc _ [] = reverse acc
        recurse acc x (y:rest) = recurse ((x,y):acc) y rest
-- >>> pair testList
-- [(199,200),(200,208),(208,210),(210,200),(200,207),(207,240),(240,269),(269,260),(260,263)]

paired = pair testList

testList :: [Int]
testList = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

-- Part 2:
main = do
    contents <- readFile "input.txt"
    let result = count $ pair $ trip $ map (\x -> read x :: Int) $ lines contents
    return result


-- Part 1:
-- main = do
--     contents <- readFile "input.txt"
--     let result = count $ pair $ map (\x -> read x :: Int) $ lines contents
--     return result
-- answer: 1676
