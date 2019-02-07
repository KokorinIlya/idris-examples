module Numbers

num : Nat -> ((a -> a) -> (a -> a))
num Z = \f => \z => z
num (S k) = \f => \z => f ((num k) f z)

inc : (n : ((a -> a) -> (a -> a))) -> ((a -> a) -> (a -> a))
inc n = \f => \z => f (n f z)

add : (n : ((a -> a) -> (a -> a))) -> (m : ((a -> a) -> (a -> a))) -> ((a -> a) -> (a -> a))
add n m = \f => \z => n f (m f z)

mul : (n : ((a -> a) -> (a -> a))) -> (m : ((a -> a) -> (a -> a))) -> ((a -> a) -> (a -> a))
mul n m = \f => \z =>
  let g = add m in
  ?x
