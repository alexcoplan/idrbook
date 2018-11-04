module Main

import System

readNum : IO (Maybe Nat)
readNum = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target gos = 
  let attstr = case gos of 
                    (S Z) => "attempt" 
                    _ => "attempts" in
  do putStr ("Take a guess (" ++ (show gos) ++ " " ++ attstr ++ "): ")
     Just gNum <- readNum
     | Nothing => do putStrLn "NaN, dude."
                     guess target gos
     case compare gNum target of
       LT => do putStrLn "Ouch! Too low, dude."
                guess target (S gos)
       EQ => putStrLn "Bang on, dude!"
       GT => do putStrLn "Ouch! Too high, dude."
                guess target (S gos)

myRepl : String -> (String -> String) -> IO ()
myRepl prompt responder = 
  do putStr prompt
     inp <- getLine
     putStrLn (responder inp)
     myRepl prompt responder

myReplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt responder = 
  do putStr prompt
     inp <- getLine
     case responder state inp of
         Nothing => pure ()
         (Just (resp, state')) => do putStr resp
                                     myReplWith state' prompt responder


main : IO ()
main = do t <- time
          guess (cast (mod t 100)) Z

