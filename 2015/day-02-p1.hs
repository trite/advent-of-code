import Data.List.Split

testing12 = splitOn 'x' "4x3x2"

-- >>> testing12

area w l h = 2*w*l + 2*w*h + 2*l*h + slack w l h
    where
        slack w l h = minimum [w*l, w*h, l*h]

-- >>> area 2 3 4
-- >>> area 1 1 10
-- 58
-- 43



-- tabulate toCheck = map area toCheck

-- >>> tabulate [()]

main = do
    contents <- readFile "day-02-input.txt"
    let result = length . lines $ contents
    return result