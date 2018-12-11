module Homework92

import Data.Vect
import Data.Fin

my_at : Fin n -> Vect n a -> a
my_at FZ (x :: xs) = x
my_at (FS k) (x :: xs) = my_at k xs

my_set_at : Fin n -> a -> Vect n a -> Vect n a
my_set_at FZ y (x :: xs) = y :: xs
my_set_at (FS k) y (x :: xs) = x :: (my_set_at k y xs)

export
swap : Vect n a -> Fin n -> Fin n -> Vect n a
swap vec i j =
   (my_set_at i val_j) $
   (my_set_at j val_i) $ vec
   where
     val_i : a
     val_i = my_at i vec

     val_j : a
     val_j = my_at j vec
