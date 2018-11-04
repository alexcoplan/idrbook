module Main
import Data.Vect

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")

vzip : Vect n a -> Vect n b -> Vect n (a,b)
vzip [] [] = []
vzip (x :: xs) (y :: ys) = (x,y) :: vzip xs ys


