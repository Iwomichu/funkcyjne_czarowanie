val :: Int -> Int -> Int
val b a = (head [i | i <- [1..], (b `mod` (a ^ i)) /= 0]) - 1

g :: Int -> Int -> [Int]
g k v = [i |  i <- [2..], val i k == v]

main = print (take 3 (g 3 1))