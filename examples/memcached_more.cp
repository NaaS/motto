include "mc_type.cp"

process MCD_req : {no_backends, req_opcode} => (mc_command/mc_command client, [mc_command/mc_command]{no_backends} backends)
  let x = ?? client
  switch x.opcode:
    req_opcode:
      let target = hash (x.key typed integer) mod no_backends
#      client => backends[target]
# The above line abbreviates the following two:
      backends[target] ! x # Note that x was "peeked" from the channel "client".
      ? client

process MCD_resp : {no_backends, req_opcode} => (mc_command/mc_command client, [mc_command/mc_command]{no_backends} backends)
  for i in 0 .. (no_backends - 1):
    if can ?? backends[i]: # i.e., "continue", don't "return" with OUT_OF_DATA otherwise
#      backends[i] => client
# The above line expands to the following:
      let y = ?? backends[i]
      client ! y
      ? backends[i]
    else: <>

#fun MCD_resp : {no_backends, req_opcode} => (mc_command/mc_command client, [mc_command/mc_command]{no_backends} backends) -> ()
#  for c in unordered backends:
#    c => client
#
#fun MCD_resp : {no_backends, req_opcode} => (mc_command/mc_command client, [mc_command/mc_command]{no_backends} backends) -> ()
#  backends => client
