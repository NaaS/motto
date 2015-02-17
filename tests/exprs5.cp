
fun F : () -> ()
  < >
  <>
# Singleton tuples are weird.
  <True >
  < True, 2 >
  <1, 2 >
  <1, 2, 4+4, [9,8, 7] >
# Need to bracket up angled brackets when they occur in tuples:
  <1, (3 > 2) >
  <1, (3 < 2)>
# because this doesn't work:
#  <1, 3 > 2 >
#  <1, 3 < 2>
#
  <>
