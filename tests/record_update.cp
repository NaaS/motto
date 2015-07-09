type rec : record
  x : integer
  b : boolean

type rec' : record
  l : string

fun F : (r : type rec', e : string) -> (type rec')
  r with l = e
  ({ x = 4, b = True} with x = 5) with b = False
# Because of "with"'s fixity, the following currently doesn't type-check well:
#  (({ x = 4, b = True} with x = 5) with b = False) = ({ x = 4, b = True} with x = 5 with b = False)

  r
