g :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
g _ p [] = return p
g h p (x:xs) = do
    temp <- h p x
    g h temp xs
    
main = print $ g (\x y -> Just (x+y)) 1 [1, 2]