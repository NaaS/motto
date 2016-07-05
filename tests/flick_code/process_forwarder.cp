fun Forwarder : (integer/- c_read, -/integer c_write) -> ()
  @: print ("loaded Forwarder").at(type_checking) :@ typed <>
  @: print ("Forwarder").at(runtime) :@ typed <>
  c_write ! ? c_read
  <>
