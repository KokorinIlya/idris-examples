module FileIO

import Data.Vect

read_from_console : IO (List String)
read_from_console = do
  cur_line <- getLine
  if (cur_line == "") then pure Nil else do
    tail <- read_from_console
    pure (cur_line :: tail)

concat_list : List String -> String
concat_list [] = ""
concat_list (x :: xs) =
  let tail_string = concat_list xs in
  x ++ "\n" ++ tail_string

read_and_save : IO ()
read_and_save = do
  putStrLn "Enter content, that should be saved"
  user_input <- read_from_console
  putStrLn "Enter file name"
  file_name <- getLine
  let content = concat_list user_input
  Right _ <- writeFile file_name content |
    Left error => do
      putStrLn "An I/O error occured, see log for description"
      putStrLn (show error)
  putStrLn "Content has been saved"

read_file_content : (file : File) ->
 IO (Either FileError (n : Nat ** Vect n String))
read_file_content file = do
  is_eof <- fEOF file
  if (is_eof) then
      pure (Right (_ ** []))
    else do
      Right cur_line <- fGetLine file | Left error => pure (Left error)
      Right (tail_lenght ** file_tail) <- read_file_content file |
        Left error => pure (Left error)
      pure (Right ((S tail_lenght) ** cur_line :: file_tail))


read_vector_from_file : (file_name : String) -> IO (n : Nat ** Vect n String)
read_vector_from_file file_name = do
  Right file <- openFile file_name Read |
    Left error => do
      putStrLn "An I/O error occured while opening file, see log for description"
      putStrLn (show error)
      pure (_ ** [])
  Right result <- read_file_content file |
    Left error => do
      putStrLn "An I/O error occured while reading file, see log for description"
      putStrLn (show error)
      pure (_ ** [])
  _ <- closeFile file
  pure result
