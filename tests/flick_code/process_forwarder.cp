fun Forwarder : (integer/- c_read, -/integer c_write) -> ()
  @: print ("loaded Forwarder").at(type_checking) :@ typed <>
  @: print ("Forwarder").at(runtime) :@ typed <>
# If we peek (rather than read) from c_read then we'll get an infinite loop,
  # since a resource isn't being consumed from the input.
#  c_write ! ?? c_read
  c_write ! ? c_read
  <>
