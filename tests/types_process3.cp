
type awesome: record
  a : string
  b : integer

process Test : {m, n} => ([type awesome/boolean] input, boolean/type awesome output)
  <>
