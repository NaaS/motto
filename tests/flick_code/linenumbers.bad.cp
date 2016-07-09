 type record_0:record  # This should error because it's a type declaration in an indented scope
  f1:string

 type record_0_2:record  # The error shouldn't be here, but on the first line, at 1:1
  f1_2:string

type record_1:record
  f1 : string
