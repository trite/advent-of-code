-- >>> filter (uncurry (/=)) (zip "00000" "0000012345")
-- []

-- startsWith :: Eq a => [a] -> [a] -> Maybe Bool
-- suffix `startsWith` prefix
--     | length prefix <= length suffix = Just(not (any (uncurry (/=)) (zip prefix suffix)))
--     | otherwise                      = Nothing
{-
>>> ("00000" ++ show 12345) `startsWith` "00000"
Just True
>>> "00001234" `startsWith` "00000"
Just False
>>> "000" `startsWith` "00001234"
Nothing
-}