include "mc_type.cp"

fun MCD : {no_backends, req_opcode} => (mc_command/mc_command client, [mc_command/mc_command]{no_backends} backends) -> ()
  let x = ?? client
  switch x.opcode:
    req_opcode:
      let target = hash (x.key typed integer) mod no_backends
      client => backends[target]
# The above line abbreviates the following two:
#      backends[target] ! x # Note that x was "peeked" from the channel "client".
#      ? client
