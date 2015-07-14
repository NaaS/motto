fun f : (x : integer, y : integer) -> (integer)
  # this fails because of shadowing -- we're binding z twice
  let z = x
  let z = y
  x + y
