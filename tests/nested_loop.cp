
fun nestlist : (nl_x : integer) -> ([[integer]])
  for nl_i1 in 0 .. nl_x
  initially nl1_acc = [] typed [[integer]]:
    let nl_i1_list =
      for nl_i2 in 0 .. nl_i1
      initially nl2_acc = [] typed [integer]:
        nl_i2 :: nl2_acc
    nl_i1_list :: nl1_acc
