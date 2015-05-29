# NOTE:
#  - combined my earlier proposal for this with ideas from the binpac spec for
#    HTTP.
#  - bytes that aren't captured (e.g., "\r\n" in the first match) aren't lost or
#    deleted; there could be a flag to require this, but it's better if they are
#    retained since this would correspond with our intended semantics: a packet
#    is pushed through a pipeline and it's changed in place, as much as is
#    possible.
#  - we cannot statically work out the maximum size of an HTTP request or
#    response. Even at runtime, using this specification, we don't try to find
#    out if its size is given (via "Content-length").

# Using the regex-abbreviated syntax inspired from binpac
type http_version: record
  _      : /HTTP\//  # "HTTP" aren't discarded, but aren't accessible
                     # instances of this type either. What follows
                     # them is accessible, through the "number" field.
  number : /[0-9]+\.[0-9]*/

# Not using the regex-abbreviated syntax: we say that the type is "string"
# and then specify what we're matching against.
type whitespace: string
  { match = "[ \t]+" }
type newline : string
    { match = "\r\n" }

type request_line: record
  method  : string
    # We restrict which methods may be used.
    { match = "GET|POST" }
  # The annotation for "whitespace" is carried over from its definition.
  _       : whitespace
  uri : string
    { match = "[^ \t]+" }
  _       : whitespace
  # The annotation for "http_version" is carried over from its definition too.
  version : http_version
  # Note that we don't tolerate any whitespace between the version and the
  # newline.
  _ : newline

type http_request: record
  request : request_line
  headers : /[^\r\n]*/
  _ : newline
  _ : newline
  payload : /.*/
