newtype Mb a = Mb ([a], [a]) deriving Show

dnp :: Mb a -> a -> Mb a
dnp (Mb tpl) new = Mb (new: fst tpl, snd tpl)

dnk :: Mb a -> a -> Mb a
dnk (Mb tpl) new = Mb (fst tpl, new: snd tpl) 

mb2list :: Mb a -> [a]
mb2list (Mb tpl) = fst tpl ++ reverse (snd tpl)

ull :: Mb a -> Mb a
ull (Mb tpl) = Mb ([], snd tpl)

upl :: Mb a -> Mb a
upl (Mb tpl) = Mb (fst tpl, [])

main = print (upl (dnk (Mb ([], [])) 2))