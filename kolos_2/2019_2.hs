
main = do
    input <- getLine 
    if head input == '.' then return () else print $ reverse input
    if head input == '.' then return () else main
