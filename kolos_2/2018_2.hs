
compr :: Monad m => [a -> m a] -> a -> m a
compr actions argument = foldr (=<<) (return argument) actions

main = print $ compr [\x->Just (x+1)] 1