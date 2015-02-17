
fun F : ([integer/integer] c; x : [boolean]) -> ()
  let y = 5
  c[y] # Channels should only be mentioned in the context of an operation -- they are not first class.
       # (This test case therefore might pass the parser, but not further checks.)
  x[y]
  x[3]
  c[{ l = 4 }]
  <>
