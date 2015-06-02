#FIXME add pragma for longest allocation?
#NOTE have left the function names (e.g., strip1) as in the original, but
#     changed arities to make parameters explicit.
#NOTE indefinite_description is the epsilon operator, in this context it returns
#     None if no satisfying witness is found.

type Token      = { string | /[^ \t\r\n]+/ }
type URI        = string {match = /[^ \t\r\n]+/}
type NewLine    = /\r?\n/
type RestOfLine = /[^\r\n]*/
type FullLine   = /[^\r\n]*\r?\n/
type Integer    = /[0-9]+/
type HexInteger = /[0-9a-zA-Z]+/
type WhiteSpace = /[ \t]+/
type OptionalWhiteSpace = /[ \t]*/

type Version = record
  _ :        /HTTP\//
  number: /[0-9]+\.[0-9]*/

type RequestLine = record
  method:  Token
  _ :      WhiteSpace
  uri:     URI
  _ :      WhiteSpace
  version: Version
  _ :      NewLine

type Request = record
  request: RequestLine
  message: Request_Message

type HeaderName  = /[^:\r\n]+/
type HeaderValue = /[^\r\n]*/

type Header = record # unit(msg: Message) {
    name :    HeaderName
    # Derivative field. applying the "uppercase" function here, rather than
    # chaining it to the declaration of "name".
    (upper_name : string { value = uppercase(name) })
    _ :       /:[\t ]*/
    content : HeaderValue
    _ :       NewLine
    (content_length : integer{width=64, signed=false} option
      { value =
          if upper_name = "CONTENT-LENGTH" then Some to_uint(content) else None })
    (transfer_encoding : string option
      { value =
          if upper_name = "TRANSFER-ENCODING" then Some content else None })
    (content_encoding : string option
      { value =
          if upper_name = "CONTENT-ENCODING" then Some content else None })
    (msg_has_body = bool
      { value = content_length <> None or transfer_encoding <> None })
    # NOTE could use "local...in" syntax to avoid code duplication below
    (content_type : (string * string) option
      { value =
          if upper_name = "CONTENT-TYPE" then
            let <ct0, _> = split1(";", content)
            let <ty0, ty1> = split1("/", ct0)
            Some <strip (uppercase ty0), strip (uppercase ty1)>
          else None })
    (content_type_parameter : string option
      { value =
          if upper_name = "CONTENT-TYPE" then
            let <_, ct1> = split1(";", content)
            Some (strip ct1)
          else None })

type DeliveryMode = variant
  EndOfData
  Length
  Multipart
  Chunked

type Request_Message = record
  headers:    Header list {separator = /\r\n/, terminator = /\r\n\r\n/}
  # NOTE if a field has a "value" attribute then it is a derivative field.
  #      if the field is bracketed, then it is a computed value during parsing,
  #      and whose metadata can be stored for the lifetime of its values.
  (content_length : integer option
    {width = 64,
     signed = false,
     value = indefinite_description (header => header.content_length <> None) headers})
  (content_encoding : string option
     #NOTE this projection idiom is used frequently here -- turn it into sugaring?
     {value = indefinite_description (header => header.content_encoding <> None) headers})
  (content_type : (string * string) option
     {value =
        let result = indefinite_description (header => header.content_type <> None) headers
        if result = None then Some <"TEXT", "PLAIN"> else result
     })
  (content_type_parameter : string
     {value = indefinite_description (header => header.content_type_parameter <> None) headers})
  (transfer_encoding : string
     {value = indefinite_description (header => header.transfer_encoding <> None) headers})
  (has_body : boolean
     {value = exists (header => header.msg_has_body) headers})

  _ : NewLine     # in binpac this was labelled, since there was a hook to it,
                  # but that's no longer needed now.

#  var is_request: bool; -- this is implicitly true, following the name of the
#                           type. i removed type parameters, i hope we can avoid
#                           them.
#  var delivery_mode: DeliveryMode; -- to avoid type parameters, i factor this
#                                      the values of this type, to create
#                                      a type for each of its values.
#                                      maybe it's better to have type parameters.
  (delivery_mode : DeliveryMode
    {value =
      # NOTE this might be used frequently -- make sugaring?
      if content_length <> None and $content_length > 0 then
        Length
      else if content_type.1 = "MULTIPART" then
        Multipart
      else if transfer_encoding <> none and $transfer_encoding "chunked" #FIXME not uppercase?
        Chunked
      else
        error "Unrecognised encoding"})
  (multipart_boundary : string option
    {value =
       if content_type_parameter = None then None
       else
         let boundary : string = match($content_type_parameter, /boundary="([^"]*)"/, 1)
         # FIXME shadowing
         let boundary : string =
           if length(boundary) = 0 then
             boundary = self.content_type_parameter.match(/boundary=([^ ;]*)/, 1)
           else boundary
         # NOTE overloaded meaning for "+"
         Some ("--" + boundary + "--\r\n")})

  body : variant
    contents : (| if has_body |) Body(delivery_mode)
    Nobody : (| all |) <>

#FIXME extract this from "body":  (use_content_length : bool = True;

