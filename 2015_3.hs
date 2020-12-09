data Klos a = Klos ([a], [a]) deriving Show

wnpk :: Klos a -> a -> Klos a
wnpk (Klos tpl) x = Klos (x:fst tpl, snd tpl)

wnkk :: Klos a -> a -> Klos a
wnkk (Klos tpl) x = Klos (fst tpl, x:snd tpl)

k2list :: Klos a -> [a]
k2list (Klos tpl) = fst tpl ++ reverse (snd tpl)

k = Klos ([1], [2])

k1 = wnkk k 3

k2 = wnpk k1 1

main = print (k2list k2)