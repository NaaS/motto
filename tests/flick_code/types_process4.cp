
type awesome: record
  a : string
  b : integer

process Test : {m} => ([type awesome/boolean]{m} input, boolean/type awesome output)
  <>
