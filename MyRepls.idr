module MyRepls

my_repl : (prompt : String) -> (on_input : String -> String) -> IO ()
my_repl promt on_input = do
  putStr promt
  user_input <- getLine
  let output = on_input user_input
  putStr output
  my_repl promt on_input
