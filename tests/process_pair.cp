process P1 : (<>/<> token)
  let x = ? token
  @: print ("P1") :@ typed <>
  token ! x

process P2 : (<>/<> token)
  let x = ? token
  @: print ("P2") :@ typed <>
  token ! x
