module VectorUtils

import Data.Vect

export
is_empty_implicit : {n: Nat} -> Vect n a -> Bool
is_empty_implicit {n = Z} vect = True
is_empty_implicit {n = (S k)} vect = False

export
is_empty_all_implicit : {n: Nat} -> {a : Type} -> Vect n a -> Bool
is_empty_all_implicit {n = Z} {a = at} vect = True
is_empty_all_implicit {n = (S k)} {a = at} vect = False

export
is_empty_all_implicit_in_decl : {n: Nat} -> {a : Type} -> Vect n a -> Bool
is_empty_all_implicit_in_decl {n = Z} vect = True
is_empty_all_implicit_in_decl {n = (S k)} vect = False

export
is_empty : Vect n a -> Bool
is_empty [] = True
is_empty (x :: xs) = False

export len_from_type : {n : Nat} -> Vect n a -> Nat
len_from_type {n} vect = n

export
total len_implicit : {n : Nat} -> Vect n a -> Nat
len_implicit {n = Z} [] = 0
len_implicit {n = (S k)} (x :: xs) = 1 + len_implicit xs

export
len : Vect n a -> Nat
len [] = 0
len (x :: xs) = 1 + len xs
