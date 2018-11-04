import Data.Vect

readToBlank : IO (List String)
readToBlank =
  do x <- getLine
     if (x == "")
        then pure []
        else do xs <- readToBlank
                pure (x :: xs)

newlineJoin : List String -> String
newlineJoin [] = ""
newlineJoin (x :: xs) = x ++ "\n" ++ (newlineJoin xs)

readAndSave : IO ()
readAndSave =
  do putStrLn "Enter list (blank line to end):"
     lines <- readToBlank
     putStrLn "Enter filename:"
     fname <- getLine
     (Right _) <- writeFile fname (newlineJoin lines) |
     (Left err) => printLn err
     putStrLn "All done."

rvfInternal : (handle : File) -> IO (n ** Vect n String)
rvfInternal handle =
  do (Right line) <- fGetLine handle |
     (Left err) => pure (_ ** [])
     if (line == "")
        then pure (_ ** [])
        else do (_ ** lines) <- rvfInternal handle
                pure (_ ** (trim line) :: lines)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename =
  do (Right handle) <- openFile filename Read |
     (Left err) => pure (_ ** [])
     result <- rvfInternal handle
     closeFile handle
     pure result

rvfMain : IO ()
rvfMain =
  do putStrLn "Enter a filename to read from:"
     fname <- getLine
     dvec <- readVectFile fname
     printLn dvec

main : IO ()
main = rvfMain

