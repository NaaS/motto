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
    { size = content_len }

# FIXME currently this isn't very nice
type http_reply: record
  { max_size = 15000,
  # FIXME this termination condition wasn't in the original example,
  #       but we need a termination condition -- check what it should be.
  termination = "\r\n" }
  dummy : <>
