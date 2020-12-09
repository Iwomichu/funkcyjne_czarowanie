sumDigits :: Int -> Int
sumDigits number
    | number < 10 = number
    | otherwise = sumDigits (sum (map (\x -> read [x] :: Int) (show number)))

getNSevensFrom :: Int -> Int -> [Int]
getNSevensFrom n from
    | n == 0 = []
    | sumDigits from == 7 = from : getNSevensFrom (n-1) (from + 1)
    | otherwise = getNSevensFrom n (from+1)

getNSevens :: Int -> [Int]
getNSevens n = getNSevensFrom n 1

main = print (getNSevens 1000)