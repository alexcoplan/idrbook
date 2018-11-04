printLonger : IO ()
printLonger = do putStr "Gimme a string: "
                 s1 <- getLine
                 putStr "Gimme another: "
                 s2 <- getLine
                 let l1 = length s1
                 let l2 = length s2
                 putStrLn (show (max l1 l2))

plDirect : IO ()
plDirect = putStr "Gimme a string: " >>= \_ =>
           getLine >>= \s1 =>
           putStr "Gimme another: " >>= \_ =>
           getLine >>= \s2 => 
           let l1 = length s1
               l2 = length s2 in
               putStrLn (show (max l1 l2))

            
