
fun F : () -> (boolean)
  let x = True
  let y = True
  let z = True
  True or (x and y or False) and (True and not z)
