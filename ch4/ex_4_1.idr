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

||| Exercise 4.1.6

||| Represents shapes
data Shape = ||| A triangle, with is base length and height
             Triangle Double Double
           | |||A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

areaIfTriangle : Shape -> Maybe Double
areaIfTriangle tri@(Triangle x y) = Just (area tri)
areaIfTriangle (Rectangle x y) = Nothing
areaIfTriangle (Circle x) = Nothing

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape) = areaIfTriangle shape
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))


