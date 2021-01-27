is3 :: Int -> Maybe Int
is3 x
    | x == fromInteger (round k) = Just x
    | otherwise = Nothing
    where
        y = fromIntegral x :: Float
        k = y ** (1 / 3)

c :: Maybe Int -> Int
c (Just x) = 2 * x + 1
c Nothing = 0
    