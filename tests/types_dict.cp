type bla : dictionary <integer * integer>

type bla : dictionary record
  key : string
  value : string

type some_type : <string * <integer * string>>

type d : dictionary type some_type


proc P : (type T1/type T2 c)
  global d : dictionary <string * string> := empty_dictionary
  <>
