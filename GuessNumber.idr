module GuessNumber

total string_to_num : List Char -> Maybe (Nat, Nat)
string_to_num Nil = Just (0, 0)
string_to_num (cur_char :: Nil) =
  if (isDigit cur_char) then
    Just (cast (ord cur_char - ord '0'), 1)
  else
    Nothing
string_to_num (cur_char :: chars_tail@(next_char :: other_chars)) =
  if (isDigit cur_char) then
    case string_to_num chars_tail of
      Just tail_res =>
        let (tail_number, tail_pow) = tail_res in
        let ten = fromIntegerNat 10 in
        let cur_pow = mult tail_pow ten in
        let cur_digit = cast (ord cur_char - ord '0') in
        Just (cur_digit * cur_pow + tail_number, cur_pow)

      Nothing => Nothing
  else
    Nothing

total read_number : IO (Maybe Nat)
read_number = do
  user_string <- getLine
  let user_chars = unpack user_string
  case string_to_num user_chars of
    Nothing => pure Nothing
    Just all_result =>
      let (result, pow) = all_result in
      pure (Just result)

guess : (target : Nat) -> IO ()
guess target = do
  putStrLn "Enter your guess"
  user_number <- read_number
  case user_number of
    Nothing => do
      putStrLn "Incorrect number, try one more time"
      guess target
    Just num => case (compare num target) of
      LT => do
        putStrLn "Your number is less"
        guess target
      EQ => putStrLn "Correct guess"

      GT => do
        putStrLn "Your number is larger"
        guess target

guess_with_counting : (target : Nat) -> Nat -> IO ()
guess_with_counting target count = do
  putStrLn "Enter your guess"
  user_number <- read_number
  case user_number of
    Nothing => do
      putStrLn "Incorrect number, try one more time"
      guess_with_counting target (S count)
    Just num => case (compare num target) of
      LT => do
        putStrLn "Your number is less"
        guess_with_counting target (S count)
      EQ => do
        putStr "Correct guess, "
        putStr (show count)
        putStrLn " incorrect numbers entered"

      GT => do
        putStrLn "Your number is larger"
        guess_with_counting target (S count)

play_guess_with_counting : (target : Nat) -> IO ()
play_guess_with_counting target = guess_with_counting target Z
