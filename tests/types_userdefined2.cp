

type t1 : record
  a : string
    { encoding = blaString,
    more = bla }
  b : integer
    { encoding = blaInteger }
  c : [integer]
    { encoding = blaListInt }

#type t2 : list type t1
#  { encoding = bla }

type t : variant
  r : record
    s : string
      { encoding = bla1 }
    i : integer
      { encoding = bla2 }
  i2 : integer
  t2 : variant
    b1 : boolean
    b2 : boolean
  s2 : string

