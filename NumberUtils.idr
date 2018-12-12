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
