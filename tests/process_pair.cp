process P1 : (<>/<> token1)
  let tkn1 = ? token1
  @: print ("loaded P1").at(type_checking) :@ typed <>
  @: print ("P1").at(runtime) :@ typed <>
  token1 ! tkn1

process P2 : (<>/<> token2)
  let tkn2 = ? token2
  @: print ("loaded P2").at(type_checking) :@ typed <>
  @: print ("P2").at(runtime) :@ typed <>
  token2 ! tkn2
