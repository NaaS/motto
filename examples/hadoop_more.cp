# Items marked with "*" need compiler extensions to handle

type k_v : record
  key : integer
  value : integer

fun Wc_node : (k_v/- x, k_v/- y, -/k_v z) -> ()
  # Instead of "vs" could have pattern matching, to give v1 and v2
#  let vs = peek_all ([x, y]) # let [v1,v2] <= [x,y]
#  let v1 = vs[0] # * check if can project from lists like this
#  let v2 = vs[1]
  let v1 = ??x
  let v2 = ??y

  if v1.key = -1 and v2.key = -1: # * i think "-" cannot be prefix
    x => z # * this syntax had been disabled
    y => _ # * this is used to discard the next item on channel y
  else: if v1.key = -1: # * not used "else if" form parses
    y => z
  else: if v2.key = -1:
    x => z

  else: if v1.key < v2.key:
    x => z
  else: if v2.key < v1.key:
    y => z
  else:
    v1.value := v1.value + v2.value # * in-place update for record field
    v1 => z
    x, y => _ # * discarding next items from multiple channels
