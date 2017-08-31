||| Represents shapes
data Shape = ||| A triangle, with is base length and height
             Triangle Double Double
           | |||A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rect : Picture
rect = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

tri : Picture
tri = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = 
  Combine (Translate 5 5 rect) 
  (Combine (Translate 35 5 circle) 
  (Translate 15 25 tri))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine p1 p2) = pictureArea p1 + pictureArea p2
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data BiggestTri = NoTri | Size Double

isTri : Shape -> Bool
isTri (Triangle x y) = True
isTri (Rectangle x y) = False
isTri (Circle x) = False

biggestSize : (left : BiggestTri) -> (right : BiggestTri) -> BiggestTri
biggestSize NoTri r = r
biggestSize l NoTri = l
biggestSize (Size x) (Size y) = if x > y then Size x else Size y

biggestTriangle : Picture -> BiggestTri
biggestTriangle (Primitive shape) = 
  if isTri shape 
     then Size (area shape) 
     else NoTri
biggestTriangle (Combine p1 p2) = 
  let p1size = biggestTriangle p1
      p2size = biggestTriangle p2
  in (biggestSize p1size p2size) 
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

