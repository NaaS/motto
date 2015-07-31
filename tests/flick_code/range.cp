
# Here we specify the type of the accumulator
fun range : (n : integer) -> ([integer])
  # NOTE descending values of x
  for x in n .. 1
  initially acc = [] typed [integer]:
    x :: acc

# Here we don't specify the accumulator's type
fun range' : (n : integer) -> ([integer])
  # NOTE descending values of x
  for x in n .. 1
  initially acc = []:
    x :: acc

# Here it's easy to infer the type of the accumulator, because it's not polymorphic.
# NOTE this is like the eta expansion of ".." syntax.
fun range_reverse : (n : integer) -> ([integer])
  for x in 1 .. n
  initially acc = [1]:
    x :: acc
