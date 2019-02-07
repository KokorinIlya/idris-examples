module StreamingActions

infixl 4 |>

(|>) : a -> (a -> b) -> b
(|>) x f = f x

check : List Int -> List Int
check list =
  list |> map (\x => x + 1)
  |> map (\x => x * 2)
  |> map (\x => x + 3)
