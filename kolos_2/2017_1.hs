zipM :: Monad m => [m a] -> [m b] -> m [(a, b)]
zipM [] _ = return []
zipM _ [] = return []
zipM (x:as) (y:bs) = do
    next <- zipM as bs
    xz <- x
    yz <- y
    return ((xz, yz) : next)

main = print $ zipM [Just 1, Just 3] [Just 2, Just 5, Just 5]