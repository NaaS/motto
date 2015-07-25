process P1 : (<>/<> token)
  let tkn = ? token
  @: print ("loaded P1").at(type_checking) :@ typed <>
  @: print ("P1").at(runtime) :@ typed <>
  token ! tkn

process P2 : (<>/<> token)
  let tkn = ? token
  @: print ("loaded P2").at(type_checking) :@ typed <>
  @: print ("P2").at(runtime) :@ typed <>
  token ! tkn
