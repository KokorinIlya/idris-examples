module InfoStorage

import Data.Vect
import Data.String

data PossibleType = IntType | StringType

get_storage_type : PossibleType -> Type
get_storage_type IntType = Integer
get_storage_type StringType = String

record DataStore where
  constructor MkData
  data_type : PossibleType
  size : Nat
  items : Vect size (get_storage_type data_type)

create_new_store : DataStore -> PossibleType -> (String, DataStore)
create_new_store old_store new_type =
  let is_empty = (size old_store) == 0 in
  let new_store = MkData new_type 0 [] in
  if is_empty then
    ("Succesfully changed type of empty data store\n", new_store)
  else
    ("Store has been erased, stores's type has been changed\n", new_store)

data ParseParamError = ErrorDescription String

get_add_param : String -> (storage_type : PossibleType) ->
 Either ParseParamError (get_storage_type storage_type)
get_add_param param storage_type =
  case storage_type of
    StringType => Right param

    IntType => case (parseInteger param) of
      Nothing => Left (ErrorDescription "Cannot parse int from argument\n")
      Just value => Right value

transform_to_string : (store_type : PossibleType) ->
  (elem : get_storage_type store_type) -> String
transform_to_string IntType elem = show elem
transform_to_string StringType elem = elem

add_elem_to_store : Vect n t -> t -> Vect (S n) t
add_elem_to_store [] x = [x]
add_elem_to_store (y :: xs) x = y :: (add_elem_to_store xs x)


main_loop : DataStore -> String -> Maybe (String, DataStore)
main_loop store input =
  let (command, params) = span (/= ' ') input in
  let param = substr 1 (length params) params in
  case command of
    "exit" => Nothing
    "set_type" =>
      case param of
        "Int" => Just (create_new_store store IntType)

        "String" => Just (create_new_store store StringType)

        _ => Just ("Unknown type\n", store)
    "add" =>
      case get_add_param param (data_type store) of
        Left (ErrorDescription description) => Just (description, store)
        Right value =>
          let new_vect = add_elem_to_store (items store) value in
          let new_store = MkData (data_type store) (S (size store)) new_vect in
          let message = "Succesfully added, index is " ++ (show (size store)) in
          Just (message ++ "\n", new_store)

    "get" =>
      case (parsePositive param) of
        Nothing => Just ("Cannot parse argument\n", store)

        Just index_int =>
          case (integerToFin index_int (size store)) of
            Nothing =>
              let message = "Index is out of range, store size is " ++ (show (size store)) in
              Just (message ++ "\n", store)
            Just bounded_index =>
              let string_elem = transform_to_string (data_type store) (index bounded_index (items store)) in
              let message = "Element is " ++ string_elem in
              Just (message ++ "\n", store)

    _ => Just ("Unknown command\n", store)

interact_with_user : IO ()
interact_with_user = replWith (MkData StringType Z []) ">>" main_loop
