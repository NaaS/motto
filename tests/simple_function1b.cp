# This was added to start testing the translator, rather than the parser.

fun f : (x : integer, y : integer) -> (integer)
  for i in 1..x
  initially acc = 0:
    y + acc
