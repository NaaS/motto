fun factorial : (n : integer) -> (integer)
  # NOTE could get a "parallel" form of this by using "unordered"?
  for x in 1 .. n
  initially acc = 1:
    x * acc
