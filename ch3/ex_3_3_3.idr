import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

zipfun : a -> Vect n a -> Vect (S n) a
zipfun x xs = x :: xs

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = 
  let xsTrans = transposeMat xs in
      zipWith zipfun x xsTrans


addVec : Num a => Vect n a -> Vect n a -> Vect n a
addVec [] [] = []
addVec (x :: xs) (y :: ys) = (x+y) :: (addVec xs ys)

addMat : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMat [] [] = []
addMat (x :: xs) (y :: ys) = 
  let added = addMat xs ys in
    (addVec x y) :: added

dot : Num el => Vect n el -> Vect n el -> el
dot [] [] = 0
dot (x :: xs) (y :: ys) = (x*y) + (dot xs ys)

matmul : Num el =>
         Vect n (Vect m el) -> Vect m (Vect p el) ->
         Vect n (Vect p el)
matmul [] ys = []
matmul (x :: xs) ys = 
  let yprime = transposeMat ys in
    let xdot = map (dot x) yprime in
      xdot :: (matmul xs ys)
