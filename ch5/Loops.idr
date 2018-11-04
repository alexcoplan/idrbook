module Main

import System

readNum : IO (Maybe Nat)
readNum = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs

countdowns : IO ()
countdowns = do putStr "Enter starting number: "
                Just startNum <- readNum
                  | Nothing => do putStrLn "Invalid input"
                                  countdowns
                countdown startNum
                putStr "Another (y/n)? "
                yn <- getLine
                if yn == "y" then countdowns
                             else pure ()

