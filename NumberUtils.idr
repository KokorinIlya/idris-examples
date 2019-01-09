module NumberUtils

import Data.Fin

export
total nat_to_fin : Nat -> (m : Nat) -> Maybe (Fin m)
nat_to_fin Z (S k) = Just FZ
nat_to_fin Z Z = Nothing
nat_to_fin (S k) Z = Nothing
nat_to_fin (S k) (S l)
  with (nat_to_fin k l)
    | Just ans = Just (FS ans)
    | Nothing = Nothing

export
total change_fin : Fin n -> (m : Nat) -> Maybe (Fin m)
change_fin FZ Z = Nothing
change_fin FZ (S k) = Just FZ
change_fin (FS k) Z = Nothing
change_fin (FS k) (S l) =
  let maybe_prev = change_fin k l in
  case maybe_prev of
    Nothing => Nothing
    Just prev => Just (FS prev)
