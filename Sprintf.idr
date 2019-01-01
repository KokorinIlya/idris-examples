module Sprintf

data Format = IntFormat Format |
  StringFormat Format |
  DoubleFormat Format |
  CharFormat Format |
  LiteralFormat String Format |
  EndFormat

chars_to_format : List Char -> Format
chars_to_format Nil = EndFormat
chars_to_format ('%' :: 'i' :: tail) =
  let tail_format = chars_to_format tail in
  IntFormat tail_format
chars_to_format ('%' :: 's' :: tail) =
  let tail_format = chars_to_format tail in
  StringFormat tail_format
chars_to_format ('%' :: 'd' :: tail) =
  let tail_format = chars_to_format tail in
  DoubleFormat tail_format
chars_to_format ('%' :: 'c' :: tail) =
  let tail_format = chars_to_format tail in
  CharFormat tail_format
chars_to_format (cur_char :: tail) =
  let tail_format = chars_to_format tail in
  case tail_format of
    LiteralFormat literal format => LiteralFormat (strCons cur_char literal) format
    format => LiteralFormat (strCons cur_char "") format

string_to_format : String -> Format
string_to_format s = chars_to_format (unpack s)

SprintfType : Format -> Type
SprintfType (IntFormat format) = Int -> SprintfType format
SprintfType (StringFormat format) = String -> SprintfType format
SprintfType (DoubleFormat format) = Double -> SprintfType format
SprintfType (CharFormat format) = Char -> SprintfType format
SprintfType (LiteralFormat _ format) = SprintfType format
SprintfType EndFormat = String

sprintf_from_format : (format : Format) -> (acc : String) -> (SprintfType format)
sprintf_from_format (IntFormat format) acc = \cur_int =>
  let cur_int_str = show cur_int in
  sprintf_from_format format (acc ++ cur_int_str)
sprintf_from_format (StringFormat format) acc =
  \cur_string => sprintf_from_format format (acc ++ cur_string)
sprintf_from_format (DoubleFormat format) acc = \cur_double =>
  let cur_double_str = show cur_double in
  sprintf_from_format format (acc ++ cur_double_str)
sprintf_from_format (CharFormat format) acc = \cur_char =>
  let cur_char_str = show cur_char in
  sprintf_from_format format (acc ++ cur_char_str)
sprintf_from_format (LiteralFormat literal format) acc =
  sprintf_from_format format (acc ++ literal)
sprintf_from_format EndFormat acc = acc

sprintf : (format : String) -> (SprintfType (string_to_format format))
sprintf format = sprintf_from_format (string_to_format format) ""
