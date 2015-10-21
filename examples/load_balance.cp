# Compile with:
#
#   ./motto.byte -I examples/ --no_type_check --disable_data_model_checks -o test examples/load_balance.cp

#include "naas.cp"
#include "http.cp"

#type http_request [|DMDMD|] : record
type http_request : record
  src_network_address : integer
  protocol : record
    TCP : record
      src_port : integer
  request_data : string
type http_response : record
  response_data : string

#fun LB : (type http_request/type http_response client, type http_response/type http_request backend; backend_choices : [type channel_metadata])
#process LB : {no_backends, backend_choices} => (http_request/http_response client, http_response/http_request backend)
process LB : {no_backends} => (http_request/http_response client, http_response/http_request backend)
  local set : boolean := False
  global backend_choices : dictionary [integer] integer := []

  # FIXME this block should be removed. It serves to make declarations that
  #       parts of the compiler can latch onto. The fields from the PDU should
  #       be translated to refer to suitable values in the runtime.
  let client_src_network_address = 0
  let client_protocol_TCP_src_port = 0
#  let backend = 0
  let backend = 0 unsafe_cast [integer]
#
#channel properties:
#  src_address : ipv4_address
#  dst_address : ipv4_address
#
#"extern" for functions? and for types?


  if set = False:
    let choice =
#      hash(client.src_network_address + client.protocol.TCP.src_port) mod no_backends
      hash(client_src_network_address + client_protocol_TCP_src_port) mod no_backends
    bind (backend, backend_choices[choice])
#    bind (backend, backend_choices)
    set := True
    <>
  else: <> #FIXME this line will be made redundant

#  client => backend
  if can ?? client:
    let y = ?? client
    backend ! y
    ? client
    <>
  else: <>

process LB_resp : (http_request/http_response client, http_response/http_request backend)
#  backend => client
  let x = ?? backend
  client ! x # Note that x was "peeked" from the channel "client".
  ? backend
