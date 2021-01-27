f :: Fractional a => [a] -> a
f [] = 1
f (a:as) = a + 1 / f as

f1 :: Fractional a => [a] -> a
f1 = foldr (\value acc -> value + 1/acc) 1


f2 :: Fractional a => [a] -> a
f2 = foldr (flip ((+) . (/) 1)) 1

f2helper :: Fractional a => a -> a -> a
f2helper = flip ((+) . (/) 1)


x = flip ((+) . (/) 1)

main = print $ f2 [1, 2]