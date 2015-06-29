
process P : (type T1/type T2 c)
  global count : integer := 0
  increment (count)
  <>

fun increment : (x : ref integer) -> ()
  x := x + 1
