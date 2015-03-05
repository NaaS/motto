

fun range : (n : integer) -> ([integer])
  for x in 1 .. n
  initially acc = []:
    x :: acc
