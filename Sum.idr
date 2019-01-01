module Sum

TypeForSum : Nat -> Type
TypeForSum Z = Int
TypeForSum (S k) = Int -> (TypeForSum k)

calculate_sum : (numbers: Nat) -> (acc : Int) -> (TypeForSum numbers)
calculate_sum Z acc = acc
calculate_sum (S k) acc = \cur_number => calculate_sum k (acc + cur_number)

sum : (numbers : Nat) -> (TypeForSum numbers)
sum Z = 0
sum (S k) = calculate_sum (S k) 0
