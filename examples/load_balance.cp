# Compile with:
#
#   ./motto.byte -I examples/ --no_type_check --disable_data_model_checks -o test examples/load_balance.cp

#include "naas.cp"
#include "http.cp"

type http_request : record
  src_network_address : integer
  protocol : record
    TCP : record
      src_port : integer
  request_data : string
type http_response : record
  response_data : string

#fun LB : (type http_request/type http_response client, type http_response/type http_request backend; backend_choices : [type channel_metadata])
fun LB : {no_backends, backend_choices} => (http_request/http_response client, http_response/http_request backend) -> ()
  local set : boolean := False

  # FIXME this block should be removed. It serves to make declarations that
  #       parts of the compiler can latch onto. The fields from the PDU should
  #       be translated to refer to suitable values in the runtime.
  let client_src_network_address = 0
  let client_protocol_TCP_src_port = 0
  let backend = 0

  if set = False:
    let choice =
#      hash(client.src_network_address + client.protocol.TCP.src_port) mod no_backends
      hash(client_src_network_address + client_protocol_TCP_src_port) mod no_backends
#    bind (backend, backend_choices[choice])
    bind (backend, backend_choices)
    set := True
  else: <> #FIXME this line will be made redundant

#  client <=> backend
  client => backend
  backend => client

