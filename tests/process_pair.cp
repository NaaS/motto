process P1 : (<>/<> token)
  let tkn1 = ? token
  @: print ("loaded P1").at(type_checking) :@ typed <>
  @: print ("P1").at(runtime) :@ typed <>
  token ! tkn1

process P2 : (<>/<> token)
  let tkn2 = ? token
  @: print ("loaded P2").at(type_checking) :@ typed <>
  @: print ("P2").at(runtime) :@ typed <>
  token ! tkn2
