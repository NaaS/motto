# Based on https://github.com/NaaS/system/blob/master/naasferatu/front-end/load_balance.naas

type http_request: record
  request : string
    { max_size = 1000,
    termination = "\r\n" }
  header : record
    { max_size = 1000,
    termination = "\r\n" }
    content_len : integer
      { size = 5,
      begin_at = "^\wContent-length\w:\w"}
  payload : string
             # FIXME probably best if i didn't use string for this -- treat as
             #       an identifier.
    { size = "http_request.header.content_len" }
#
type http_reply: string
#  { max_size = 15000,
#  termination = "\r\n" }
