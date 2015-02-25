# This differs from types_userdefined2.cp in field "c" below

type t1 : record
  a : string
    { encoding = blaString,
    more = bla }
  b : integer
    { encoding = blaInteger }
  c : [integer]
    { encoding = blaListInt }

