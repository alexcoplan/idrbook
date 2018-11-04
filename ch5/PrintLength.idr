printLength : IO ()
printLength = getLine >>= 
  \inp => let len = length inp in
              putStrLn (show len)

plv2 : IO ()
plv2 = do putStr "Input string: "
          inp <- getLine
          let len = length inp
          putStrLn (show len)

