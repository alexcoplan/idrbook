module ex_2

export
palindrome : Nat -> String -> Bool
palindrome n x =
  let xl = toLower x
  in (length x > n) && xl == (reverse xl)

export
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten lst = take 10 (reverse (sort lst))

over_length : Nat -> List String -> Nat
over_length len lst = sum (map over lst)
  where
    over : String -> Nat
    over s = if length s > len then 1 else 0
