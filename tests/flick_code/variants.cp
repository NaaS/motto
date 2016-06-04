type vary : variant
  c : integer
  d : <> # Unit-typed fields are redundant. They can be erased during translation.

type vary2 : variant
  e : <>

fun F_variants : (x : type vary) -> ()
  switch x:
    c (y):
      <>
    d (z): # Note that z must be of unit type
      z
#    e (z): # Type checker rejects this because 'e(.)' expressions are disjuncts
#           # of a different type.
#      z
