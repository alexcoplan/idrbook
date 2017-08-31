import Data.Vect

stridxs : Vect n String -> Vect n String
stridxs xs = striter 0 xs
where
  striter : Nat -> Vect n String -> Vect n String
  striter _ [] = []
  striter k (x :: xs) = (show k ++ ": " ++ x) :: (striter (S k) xs)

strjoin : String -> Vect n String -> String
strjoin sep [] = ""
strjoin sep (x :: xs) = foldl ffun x xs
where
  ffun : String -> String -> String
  ffun prev x = prev ++ sep ++ x



