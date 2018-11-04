readNum : IO (Maybe Nat)
readNum = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

readNums : IO (Maybe (Nat, Nat))
readNums = 
  do Just num1_ok <- readNum | Nothing => pure Nothing
     Just num2_ok <- readNum | Nothing => pure Nothing
     pure (Just (num1_ok, num2_ok))

