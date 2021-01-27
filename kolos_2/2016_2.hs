type Rejestr = Maybe Int

inc :: Rejestr -> Rejestr
inc (Just value)
    | value == 10 = Nothing
    | otherwise = Just (value + 1)
inc _ = Nothing 

dec :: Rejestr -> Rejestr
dec (Just value)
    | value == -10 = Nothing
    | otherwise = Just (value -1)
dec _ = Nothing 

main = print $ do
    let rejestrStartowy = Just 10
    let blednyRejestr = inc rejestrStartowy
    inc $ inc $ inc blednyRejestr