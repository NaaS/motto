

fun F : () -> ()
  map x in [1,2,3]:
    x + 1

  map x in [1,2,3]:
    x * 2
  map x in 1..3:
    x - 2

  for x in (2 .. (y + 4)) :
    <>

  for x in 2 .. (y + 4)
  initially acc = 0:
    <>
    x + acc

# FIXME should we index lists?
#  let l = [1, 2, 3]
#
#  for x in l[4]
#  initially y = 0:
#    if x > y:
#      x
#    else:
#      y
  <>

# Test cases involving unordered collections
fun G : () -> ()
  map x in unordered [1,2,3]:
    x + 1

  map x in unordered [1,2,3]:
    x * 2
  map x in 1..3:
    x - 2

  for x in unordered (2 .. (y + 4)) :
    <>

  for x in unordered 2 .. (y + 4)
  initially acc = 0:
    <>
    x + acc

# FIXME should we index lists?
#  for x in unordered l[4]
#  initially y = 0:
#    if x > y:
#      x
#    else:
#      y
  <>
