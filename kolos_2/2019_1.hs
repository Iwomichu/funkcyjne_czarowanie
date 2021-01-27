divBy3 :: Int -> Bool 
divBy3 = (== 0) . flip mod 3

f :: [[Int]] -> Int
f = length . x 

x :: [[Int]] -> [Int]
x = filter divBy3 . map c

c :: [Int] -> Int
c = length . filter divBy3

g :: [[Int]] -> Int 
g = length . (filter divBy3 . map (length . filter divBy3))

main = print $ f [[1,2,3], [3,3,3]]