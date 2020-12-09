-- zadanie mozna sprowadzic do wygenerowania n/2

generateBinarySequenceOfLength :: Int -> [String]
generateBinarySequenceOfLength len
  | len > 0 = map (++ "a") (generateBinarySequenceOfLength (len-1)) ++ map (++ "b") (generateBinarySequenceOfLength (len-1))
  | otherwise = [""]

countLetterOccurences :: Char -> String -> Int
countLetterOccurences letter word = length (filter (== letter) word)

isBalanced :: String -> Bool
isBalanced word = countLetterOccurences 'a' word == countLetterOccurences 'b' word

bp :: Int -> [String]
bp n = map (\x -> x ++ reverse x) (filter isBalanced (generateBinarySequenceOfLength (n `div` 2)))

main=print (bp 8)
