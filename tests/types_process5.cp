
type awesome: record
  a : string
  b : integer

process Test : {m, n, o} => ([type awesome/boolean]{m} input, list{m} boolean/type awesome output)
  <>
