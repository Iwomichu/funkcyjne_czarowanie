fmappH :: [a -> a] -> a -> [a]
fmappH functions startingValue = scanl (flip id) startingValue functions

fmappH2 :: [a -> a] -> a -> [a]
fmappH2 = flip (scanl (flip id))

fmappH3 :: [a -> a] -> a -> [a]
fmappH3 = flip (scanl (flip id))
main = print $ fmappH3 [(+1)] 1