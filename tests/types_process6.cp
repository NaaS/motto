
type awesome: record
  a : string
  b : integer

proc Test : {m, n, o} => ([-/boolean]{m} output, list{m} boolean/- input)
  # Empty process

proc Test1 : (integer/- input, -/list integer output)
  # Empty process

# Cannot have list -, or [-], or a record/variant containing an empty type,
# nor a type synonym for the empty type.
