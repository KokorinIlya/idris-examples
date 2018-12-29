module DependentTypeExample

choose_type : Bool -> Type
choose_type True = Integer
choose_type False = String

get_choosen_type : (chooser: Bool) -> (choose_type chooser)
get_choosen_type chooser = case chooser of
                              True => 42
                              False => "42"

export
total get42 : Bool -> String
get42 True = show (get_choosen_type True)
get42 False = get_choosen_type False

export
total value_to_canonical_string : (is_int : Bool) -> (
  case is_int of
    False => String
    True => Int
) -> String
value_to_canonical_string False x = trim x
value_to_canonical_string True x = cast x
