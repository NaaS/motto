

type l : [[[integer]]]

#NOTE types cannot be recursive, and they occupy a separate namespace from that
# of constants.
type r : record
  x : type l
  y : [boolean]
  z : [type l]

process P : {m} => ([[integer]/-]{m} input, -/[integer]{m} output)
  <>


