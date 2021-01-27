ce :: [[Int]] -> [Int]
ce = concat . filter (even . length)

main = print $ ce [[1, 2], [2, 3, 4], [3, 4]]