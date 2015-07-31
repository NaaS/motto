
fun F : (y1 : integer, y2 : integer, z : [integer], x2 : [integer]) -> ()
  let x : [integer] = []
  let x_annotated_too : [integer] = [] typed [integer]
  let x_annotated_too' : [integer] = ([] typed [integer])

  # NOTE none of these make it into the symbol table, since they are bracketed
  #      out of the containing function's scope! They are type checked, then discarded.
  (let x' : [integer] = ([] typed [integer])) typed [integer]
  (let x'' : [integer] = []) typed [integer]
  (let x''' = ([] typed [integer])) typed [integer]
  (let x'''' = []) typed [integer]

  x = y1 :: y2 :: z
  x @ x2 = y2 :: z
  [ ] typed list boolean
  [] typed list list list string
  [1, 2,4, 1+4]

  @: print (symbol_table) :@ typed <>

  <>
