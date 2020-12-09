isValid :: String -> Bool
isValid word
    | length word < 2 = True
    | all (== 'a') word = True
    | otherwise = False

transform :: String -> String
transform [x] = [x]
transform [x,y]
    | x == 'a' && y == 'a' = "aaa"
    | x == 'a' && y == 'b' = "a"
    | x == 'b' && y == 'a' = "b"
    | otherwise = "a"
transform (x:y:xs) = transform [x,y] ++ transform xs

transformUntilValidCall :: Int -> String -> Int
transformUntilValidCall call word
    | isValid word = call
    | otherwise = transformUntilValidCall (call+1) (transform word)

dlugosc :: String -> Int
dlugosc = transformUntilValidCall 0


main = print (dlugosc "babba")