module Main

import Average

showAvg : String -> String
showAvg str = "The average word length is: " 
              ++ show (average str) ++ "\n"

main : IO ()
main = repl "Enter a string: " 
            showAvg
