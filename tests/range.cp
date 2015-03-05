
fun range : (n : integer) -> ([integer])
  # NOTE descending values of x
  for x in n .. 1
  initially acc = []:
    x :: acc

# NOTE this is like the eta expansion of ".." syntax.
fun range_reverse : (n : integer) -> ([integer])
  for x in 1 .. n
  initially acc = []:
    x :: acc
