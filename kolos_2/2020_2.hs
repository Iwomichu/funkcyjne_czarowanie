data OccurenceCountPair = OccurenceCountPair Char Int deriving Show

occurenceCount :: String -> [OccurenceCountPair]
occurenceCount line = map (\uniq -> OccurenceCountPair uniq (length (filter (== uniq) line))) unique
    where
        unique = getUnique line

getUnique :: String -> [Char]
getUnique = foldl (\acc new -> if new `elem` acc then acc else new:acc) []

isTerminal :: String -> Bool 
isTerminal = (== '.') . head 

inputLoop :: IO ()
inputLoop = do
    input <- getLine
    if isTerminal input then return () else print $ occurenceCount input
    if isTerminal input then return () else inputLoop

main = inputLoop