# NOTE:
#  - the "^" anchor is relative to what has been matched so far, and not
#    absolutely anchored to the start of the bytestream.
#  - bytes that aren't captured (e.g., "\r\n" in the first match) aren't lost or
#    deleted; there could be a flag to require this.

type http_request: record
  request : string
    { match = "^(.+?)\r\n" }  # "\r\n" aren't discarded, but aren't accessible
                              # instances of this type either. What precedes
                              # them is accessible, through the "request" field.
  _ : string
    { match = "^.+?Content-length\w*:\w*" }
  content_len : integer # We start reading this value from where the previous
    { signed = false,   # read/match left off.
    max_byte_size = 8,
    encoding = ASCII}
  _ : string
    { match = "^.+?\r\n\r\n" }
  payload : string
    { byte_size = content_len } # Length dependency on an earlier field.

