data Rd a = Rd a [Rd a] deriving Show

el :: Eq a => Rd a -> a -> Bool
el (Rd y []) x = x == y
el (Rd y ys) x = x == y || any (`el` x) ys

subst :: Eq a => a -> a -> Rd a -> Rd a
subst prev new (Rd curr [])
    | curr == prev = Rd new []
    | otherwise = Rd curr []
subst prev new (Rd curr currs)
    | curr == prev = Rd new (map (subst prev new) currs)
    | otherwise = Rd curr (map (subst prev new) currs)

rd2list :: Rd a -> [a]
rd2list (Rd root kids) = root:concatMap rd2list kids


main = print (rd2list(subst 5 5 (Rd 6 [Rd 5 [Rd 1 [], Rd 11 []], Rd 3 []])))

