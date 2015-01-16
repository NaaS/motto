
type awesome: record
  a : string
  b : integer

proc Test : {m} => ([type awesome/boolean]{m} input, boolean/type awesome output)
  # Empty process
