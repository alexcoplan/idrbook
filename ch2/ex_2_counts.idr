import ex_2

show_counts : String -> String
show_counts s = "Counts: " 
                ++ show (counts s) ++ "\n"

main : IO ()
main = repl "Enter a string: " show_counts
