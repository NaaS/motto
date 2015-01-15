
# More complex type structures, including lists.

type bla : list record
  a : unit
  b : boolean
  c : variant
    d : integer
    e : string
  f : list variant
    g : boolean
    h : string
       # And here's some random comment.
  i : record
    j : string
    k : list record
      l : integer
 # And here's some other random comment.
      m : boolean
    n : record
      o : string
      p : string
  q : boolean

