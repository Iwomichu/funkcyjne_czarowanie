cykle :: [a] -> [[a]]
cykle lista = foldl (\(y:ys) _ -> (tail y ++ [head y]):y:ys) [lista] [1..length lista - 1]

main = print (cykle [1,2,3])
