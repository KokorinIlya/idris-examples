module DependentTypeIO

import Data.Vect

data VectOfUnkownLength : Type  where
  MkVectOfUnkownLength : (k : Nat) -> Vect k String -> VectOfUnkownLength

read_vect_desugarized : IO VectOfUnkownLength
read_vect_desugarized = do
  cur_line <- getLine
  if (cur_line == "") then
      pure (MkVectOfUnkownLength 0 [])
    else do
      (MkVectOfUnkownLength len vect) <- read_vect_desugarized
      pure (MkVectOfUnkownLength (S len) (cur_line :: vect))

read_and_zip_vectors_desugarized : IO()
read_and_zip_vectors_desugarized = do
  putStrLn "Print first vector"
  (MkVectOfUnkownLength first_len first_vect) <- read_vect_desugarized
  putStrLn "Print second vector"
  (MkVectOfUnkownLength second_len second_vect) <- read_vect_desugarized
  case exactLength first_len second_vect of
    Nothing => putStrLn "Cannot zip two vectors with different lengths"

    Just new_second_vect => do
      putStrLn "Zipped vector is"
      let zipped = zip first_vect new_second_vect
      printLn zipped

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
