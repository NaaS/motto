# This is used to test the inliner
fun test_fun : {x} => (y : integer) -> (integer)
  let z = x + y
  let z' = z
  let z'' = z'
  let z''' = z''
  z'''
