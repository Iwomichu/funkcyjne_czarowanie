sumPoweredDigits :: Int -> Int -> Int
sumPoweredDigits power number = sum (map (\x -> (read [x] :: Int) ^ power) (show number))

pownum :: Int -> [Int]
pownum pow = [x | x <- [1..], sumPoweredDigits pow x == x]

main = print (take 5 (pownum 8))