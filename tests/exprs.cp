
fun F : () -> ()
  True
  True or not False

  let x = True
  let x' = x
  let y = not x'
  let z = x or y
  let y'' = z and (not y)

  if False: x' else: y
  if x or y: True else: False

  if x and not True: y''
  else: z

  if y or x and z:
    True
  else: False or x

  if y or x and z:
    True
  else:
    False or x

  x

  x := True
  x := True or False

  if x:
    x := False
  else:
    x := True

  let x = True
  let y : boolean = False or (x and not y)
  <>
