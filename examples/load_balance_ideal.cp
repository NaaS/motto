include "naas.cp"

# How I think a load balancer should be defined: choice is parametric, as is the
# number and nature of backends.

proc LB : (type http_request/type http_response client, [type http_response/type http_request] backend)
  let choice = hash(client.src_network_address + client.protocol.TCP.src_port) mod len(backend)
  client <=> backend[choice]

