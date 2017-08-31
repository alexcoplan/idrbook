data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

append : Vect n el -> Vect m el -> Vect (n + m) el
append [] ys = ys
append (x :: xs) ys = x :: (append xs ys)

zip : Vect n a -> Vect n b -> Vect n (a,b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
