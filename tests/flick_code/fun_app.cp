

fun F1 : (x : integer) -> ()
  <>

fun F2 : (x : integer) -> ()
  <>

fun F3 : (f31 : <integer * boolean>, f32 : [integer], f33 : boolean, f34 : [integer]) -> ()
  <>

fun F6 : (f6 : boolean) -> (integer)
  if f6: 1
  else: 0

fun F5' : (f5' : integer) -> (integer)
  f5' + 1

fun F4 : (f41 : <>, f42 : <>, f43 : integer, f44 : <integer * boolean>, f45 : [integer], f46 : boolean, f47 : [boolean]) -> ()
  <>

fun F5 : () -> ()
  F1(3)
#  F2 ()
  F2 (4)
  F3 (<1, False>, [1, 8], True, [])
  F3 (<1, False>, [1, 8], True, [] typed [integer])
  F4 (F1(3), F2(3), F5'(F6(False)), <1, False>, [1, 8], True, [] typed [boolean])


  3.F1()
#  <1, False>.F3([1, 8], True, [])   # Wrong type
  [].F3(<1, False>, [1, 8], True)
  [] typed [integer] . F3(<1, False>, [1, 8], True)

  [] . F4 (3.F1(), 3.F2(), F5'(F6(False)), <1, False>, [1, 8], True)

  <1, False> . F4 (3.F1(), 3.F2(), F5'(F6(False)), _, [1, 8], True, [])

#  <1, False> . F4 (3.F1(), 3.F2(), F5'(F6(False)), _, [1, 8], True, _) # type error
#  <1, False> . F4 (3.F1(), 3.F2(), F5'(F6(False)), <1, False>, [1, 8], True, []) # type error -- too many parameters

  [True, False, False] . F4 (3.F1(), 3.F2(), F5'(F6(False)), <1, False>, [], True, _)
  [1, 7] . F4 (3.F1(), 3.F2(), F5'(F6(False)), <1, False>, _, True, [])
  # filling multiple holes -- polymorphically!
  [] . F4 (3.F1(), 3.F2(), F5'(F6(False)), <1, False>, _, True, _)
  # filling nested holes
  7 . F4 (_.F1(), F2(_), F5'(F6(False)), <_, False>, [_, 4], True, [True])

#  _ . F4 (_.F1(), F2(_), F5'(F6(False)), <_, False>, [_, 4], True, [True]) #this should fail

  <>
