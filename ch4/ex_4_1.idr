data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
  case (compare x val) of
       LT => Node (insert x left) val right
       EQ => orig
       GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)
  
app : (left : List a) -> (right : List a) -> List a
app [] right = right
app (x :: xs) right = x :: (app xs right)
  
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node l x r) = 
  let left = treeToList l
      right = treeToList r
  in (app left (x::right))

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add e1 e2) = (evaluate e1) + (evaluate e2)
evaluate (Sub e1 e2) = (evaluate e1) - (evaluate e2)
evaluate (Mul e1 e2) = (evaluate e1) * (evaluate e2)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe l@(Just x) r@(Just y) = 
  case (compare x y) of
    LT => r
    EQ => r
    GT => l


