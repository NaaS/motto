type vary : variant
  c : integer
  d : <>

type vary2 : variant
  e : <>

fun F : (x : type vary) -> ()
  switch x:
    c (y):
      <>
    d (z): # Note that z must be of unit type
      z
#    e (z): # Type checker rejects this
#      z
