

fun F1 : (x : integer) -> ()
  <>

fun F2 : () -> ()
  F1(3)
  F2 ()
  F3 (<1, False>, [1, 8], True, [])
  F4 (F1(3), F2(), F5(F6(False)), <1, False>, [1, 8], True, [])

  3.F1()
  <1, False>.F3([1, 8], True, [])
  3.F1().F4 (F2(), F5(F6(False)), <1, False>, [1, 8], True, [])

  <>
