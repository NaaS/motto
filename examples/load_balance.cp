include "naas.cp"
include "http.cp"

proc LB : (type http_request/type http_response client, type http_response/type http_request backend; backend_choices : [type channel_metadata])
  local set := False
  if set = False:
    let choice =
      hash(client.src_network_address + client.protocol.TCP.src_port) mod len(backend_choices)
    bind (backend, backend_choices[choice])
    set := True
  else: <> #FIXME this line will be made redundant
  client <=> backend

