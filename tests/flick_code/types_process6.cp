
type awesome: record
  a : string
  b : integer

process Test : {m, n, o} => ([-/boolean]{m} output, list{m} boolean/- input)
  <>

process Test1 : (integer/- input, -/list integer output)
  <>

# Cannot have list -, or [-], or a record/variant containing an empty type,
# nor a type synonym for the empty type.
