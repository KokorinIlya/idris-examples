module Mappers

import Data.Vect

export
total map_vector : {n : Nat} -> Vect n a -> (a -> b) -> Vect n b
map_vector {n = Z} vect f = []
map_vector {n = (S k)} (x :: xs) f = (f x) :: (map_vector xs f)

export
total map_list : List a -> (a -> b) -> List b
map_list [] f = []
map_list (x :: xs) f = (f x) :: (map_list xs f)
