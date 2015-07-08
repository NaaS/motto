
type rec : record
  test : integer
  a2 : boolean
  b3 : <integer * boolean * [integer]>

fun F : () -> ()
  { test = 3, a2 = True, b3 = <1, False, [3]>}

  { test = 3,
  a2 = True,
  b3 = <1, False, [3]>}

  <>
