subst :: (Foldable t, Eq a) => (a, a) -> t a -> [a]
subst (old, new) = foldl (\acc letter -> acc ++ [if letter == old then new else letter]) []
 
repl :: Eq a => [a] -> [(a, a)] -> [a]
repl = foldl (flip subst)

main = print (repl "alamakota" [('a','u'), ('o','e')])