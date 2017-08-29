import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

app : (x : a) -> (revd : List a) -> List a
app x [] = [x]
app x (y :: xs) = y :: (app x xs)

my_rev : List a -> List a
my_rev [] = []
my_rev (x :: xs) = let revd = my_rev xs in
                       app x revd

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vmap : (a -> b) -> Vect n a -> Vect n b
my_vmap f [] = []
my_vmap f (x :: xs) = f x :: my_vmap f xs


