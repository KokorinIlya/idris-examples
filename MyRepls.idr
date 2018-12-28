module MyRepls

my_repl : (prompt : String) -> (on_input : String -> String) -> IO ()
my_repl promt on_input = do
  putStr promt
  user_input <- getLine
  let output = on_input user_input
  putStr output
  my_repl promt on_input

my_repl_with : (state : a) -> (prompt : String) ->
 (on_input : a -> String -> Maybe (String, a)) -> IO ()
my_repl_with state prompt on_input = do
  putStr prompt
  user_input <- getLine
  let maybe_result = on_input state user_input
  case maybe_result of
    Nothing => putStrLn "Exiting REPL..."

    Just (output, new_state) => do
      putStr output
      my_repl_with new_state prompt on_input

on_input_for_tests : Integer -> String -> Maybe (String, Integer)
on_input_for_tests k input = if (input == "inc") then
    let incremented = k + 1 in
    Just ("Number is " ++ show incremented ++ "\n", incremented)
  else if (input == "dec") then
    let decremented = k - 1 in
    Just ("Number is " ++ show decremented ++ "\n", decremented)
  else Nothing
