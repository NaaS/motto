type T1 : record
  _ : <>
type T2 : record
  _ : <>

process P : (type T1/type T2 c)
  global count : integer := 0
  increment (count)
  <>

# FIXME need coercion from ref to container type on RHS
#fun increment : (x : ref integer) -> ()
#  x := x + 1
