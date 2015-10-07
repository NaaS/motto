# Items marked with "*" need compiler extensions to handle
# Experimental -- run with
#   ./motto.byte --disable_inlining --disable_var_erasure --no_type_check \
#     -o test examples/hadoop_more1.cp

type k_v : record
  key : integer
    {hadoop_vint = true}
  value : integer
    {hadoop_vint = true}

#fun peek : (type k_v/- c) -> (type k_v)
#  ? c # should be a peek -- not a receive
fun peek : (c''' : type k_v) -> (type k_v)
  c'''
fun send : (c'' : type k_v, x' : type k_v) -> ()
  c''
  <> #x' # FIXME
fun consume : (c' : type k_v) -> (type k_v)
  c'

#fun Wc_node : (type k_v/- x, type k_v/- y, -/type k_v z) -> ()
fun Wc_node : (type k_v/- chan; x : type k_v, y : type k_v, z : type k_v) -> ()
  # Instead of "vs" could have pattern matching, to give v1 and v2
#  let vs = peek_all () #([x, y])
#  let v1 = vs[0] # * check if can project from lists like this
#  let v2 = vs[1]
#  let v1 = peek (x)
#  let v2 = peek (y)
  let v1 : type k_v = ?? chan
  let v2 : type k_v = ?? chan

#  if v1 > v2:
#    <>
#  else: <>

#  chan ! ? chan
#  x
#  z
#  consume (x)
##  send (z, consume (x))

  if v1.key = 0-1 and v2.key = 0-1: # * i think "-" cannot be prefix
#    z ! ? x
    ? chan 
    chan ! 5 # FIXME test
    send (z, consume (x))
    consume (y)
    <>
  else: if v1.key = 0-1:
#    z ! ? y
    send (z, consume (y))
#    <>
  else: if v2.key = 0-1:
#    z ! ? x
    send (z, consume (x))
#    <>
  else: if v1.key < v2.key:
#    z ! ? x
    send (z, consume (x))
#    <>
  else: if v2.key < v1.key:
#    z ! ? y
    send (z, consume (y))
#    <>
  else:
#    z ! v1 with value = v1.value + v2.value
#    send (z, 5) # need record update
    send (z, v1 with value = v1.value + v2.value)
#    ? x
#    ? y
    consume (x)
    consume (y)
    <>

#    # NOTE experimental area
#    #x := y
#    let v = x
  <> #x # FIXME should be <>
