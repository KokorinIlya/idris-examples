module DependentTypeIO

import Data.Vect

read_vect_of_unknown_length : IO (n : Nat ** Vect n String)
read_vect_of_unknown_length = do
  cur_line <- getLine
  if (cur_line == "") then
      pure (_ ** [])
    else do
      (tail_length ** tail) <- read_vect_of_unknown_length
      pure (S tail_length ** cur_line :: tail)

read_and_zip_vectors : IO()
read_and_zip_vectors = do
  putStrLn "Print first vector"
  (first_len ** first) <- read_vect_of_unknown_length
  putStrLn "Print second vector"
  (second_len ** second) <- read_vect_of_unknown_length
  case exactLength first_len second of
    Nothing => putStrLn "Cannot zip two vectors with different lengths"

    Just new_second => do
      putStrLn "Zipped vector is"
      let zipped = zip first new_second
      printLn zipped
