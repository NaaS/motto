
fun F : () -> ()
  let x = 0
  let x' = True
  let y = 1
  let z = 2

  x = y
  x < y
  x > y
  x' = (y > z) or (y < z)

  x' = 4 > z
  x' = (4 > z) or (y < z)
  x' = 4 > z or (y < z)
  x' = 4 > z or y < z
  x' = 4 > z or y < z and 3 + 4 * x = y / 4 - 2
  4 mod 3 = 1 mod 3
  3 = abs (0 - 3)
  <>
