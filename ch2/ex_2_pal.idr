import ex_2

show_pal : String -> String
show_pal s = show (palindrome 5 s) ++ "\n"

main : IO ()
main = repl "Enter a string: " show_pal
