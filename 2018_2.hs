data Expr a = Value a| Add (Expr a) (Expr a)| Mul (Expr a) (Expr a)| Sub (Expr a) (Expr a)| P

data NodeType a = Val a| AddType| MulType| Subtype| PType  deriving Show

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

singleton :: a -> Tree a  
singleton x = Node x Nil Nil

buildTreeFromExpr :: Expr a -> Tree (NodeType a)
buildTreeFromExpr e =
    case e of
        Value x -> singleton (Val x)
        Add x y -> Node AddType (buildTreeFromExpr x) (buildTreeFromExpr y)
        Mul x y -> Node MulType (buildTreeFromExpr x) (buildTreeFromExpr y)
        Sub x y -> Node Subtype (buildTreeFromExpr x) (buildTreeFromExpr y)
        P -> singleton PType

compareExprTrees :: Tree (NodeType a) -> Tree (NodeType a) -> Bool
compareExprTrees Nil Nil = True
compareExprTrees _ Nil = False
compareExprTrees Nil _ = False
compareExprTrees (Node x x_left x_right) (Node y y_left y_right) = compareExprNodeTypes x y && compareExprTrees x_left y_left && compareExprTrees x_right y_right

compareExprNodeTypes ::  NodeType a -> NodeType a -> Bool
compareExprNodeTypes PType _ = True
compareExprNodeTypes _ PType = True
compareExprNodeTypes (Val x) (Val y) = True -- x == y
compareExprNodeTypes AddType AddType = True
compareExprNodeTypes MulType MulType = True
compareExprNodeTypes Subtype Subtype = True
compareExprNodeTypes _ _ = False
        

t1 = buildTreeFromExpr (Add (Mul (Value (3::Int)) (Value 2)) (Value 1))

t2 = buildTreeFromExpr (Add (Mul (Value (3::Int)) (Value 2)) (Add (Value 13) (P)))

main = print (compareExprTrees t1 t2)