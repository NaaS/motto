
type awesome: record
  a : string
  b : integer

proc Test : {m, n, o} => ([type awesome/boolean]{m} input, list{m} boolean/type awesome output)
  <>
