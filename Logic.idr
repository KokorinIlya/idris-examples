module Logic

true : a -> a -> a
true x y = x

false : a -> a -> a
false x y = y

my_if : (a -> a -> a) -> a -> a -> a
my_if condition first second = condition first second

and : (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a)
and a b = \x => \y => my_if a (my_if b x y) y

or : (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a)
or a b = \x => \y => my_if a x (my_if b x y)

not : (b : (a -> a -> a)) -> (a -> a -> a)
not b = \x => \y => b y x
