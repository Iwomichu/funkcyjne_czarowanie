slice :: Int -> Int -> String -> String
slice from to xs = take (to - from + 1) (drop from xs)

postfix:: String -> [String] -> Int -> [String]
postfix word acc i = slice 0 i word : acc

t1 :: String -> [String]
t1 word = reverse (foldl (postfix word) [] [0..length word-1])

prefix:: String -> [String] -> Int -> [String]
prefix word acc i = slice i (length word - 1) word : acc

t2 :: String -> [String]
t2 word = reverse (foldl (prefix word) [] [1..length word-1]) 

t3 :: String -> [String]
t3 word = t1 word ++ t2 word

main = print (t3 "test")