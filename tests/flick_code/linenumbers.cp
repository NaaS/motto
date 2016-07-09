type record_0:record  # The next type is preceeded by multiple blank lines
 f1:string



type record_0_2:record  # The next type is preceeded by a single blank line
 f1_2:string

type record_1:record
  f1 : string
  # The following line consists of 5 spaces, and the following of 1
     
 
# This comment isn't aligned with indentation
  f2 : string

type record_example: record # This type contains nested scope
  field1 : string
  field2 : boolean
  field3 : integer
  field_4 : record
    f1 : string
    f2 : integer # Followed by 2 empty lines, 3 lines of a single space, and 1 empty line


 
 
 

type record_3:record
  f1:string # There are trailing lines after this type


