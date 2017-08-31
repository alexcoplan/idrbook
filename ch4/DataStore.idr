module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
where
  addToData : Vect old String -> Vect (S old) String
  addToData [] = [newitem]
  addToData (item :: itemz) = item :: addToData itemz

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

total parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" s = Just (Search s)
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = 
  let store_items = items store in
    case integerToFin pos (size store) of
         Nothing => Just ("Out of range\n", store)
         Just id => Just (index id store_items ++ "\n", store)

stridxs : Vect n String -> Vect n String
stridxs xs = striter 0 xs
where
  striter : Nat -> Vect n String -> Vect n String
  striter _ [] = []
  striter k (x :: xs) = (show k ++ ": " ++ x) :: (striter (S k) xs)

strjoin : String -> Vect n String -> String
strjoin sep [] = ""
strjoin sep (x :: xs) = foldl ffun x xs
where
  ffun : String -> String -> String
  ffun prev x = prev ++ sep ++ x

findItem : (str : String) -> (store : DataStore) -> Maybe (String, DataStore)
findItem str store = 
  let store_items = items store 
      filtered = snd (Vect.filter (Strings.isInfixOf str) (stridxs store_items))
      in Just (strjoin "\n" filtered ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = 
  case (parse inp) of
       Nothing => Just ("Invalid command\n", store)
       (Just (Add item)) => 
          Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
       (Just (Get pos)) => getEntry pos store 
       (Just (Search str)) => findItem str store 
       (Just Size) => Just (show (size store) ++ "\n", store)
       (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

