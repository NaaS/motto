# Translation of specs of the memcached type used in the paper draft,
# to a form that the compiler can handle.

#type req : record
#   _ : bytes(1)
#   opcode : bytes(1)
#   keylen : uint_16
#   _ : bytes(20)
#   key : bytes(keylen)
#
#type reply : record
#   _ : bytes(1)
#   opcode : bytes(1)
#   keylen : uint_16
#   extraslen: uint_8
#   _ : bytes(3)
#   bodylen : uint_64
#   _ : bytes(12)
#   extras : bytes(extraslen)
#   key: bytes(keylen)
#   value: bytes(bodylen-extraslen-keylen)

type req : record
   IGNORE_ : string
     { byte_size = 1 }

   opcode : string
     { byte_size = 1 }

   keylen : integer
     { signed = false,
     byte_size = 2 }

   IGNORE_ : string
     { byte_size = 20 }

   key : string
     { byte_size = keylen }



type reply : record
   IGNORE_ : string
     { byte_size = 1 }

   opcode : string
     { byte_size = 1 }

   keylen : integer
     { signed = false,
     byte_size = 2 }


   # This field isn't in the req type.

   extraslen : integer
     { signed = false,
     byte_size = 1 }

   # The rest differs from the req type too.

   IGNORE_ : string
     { byte_size = 3 }

   bodylen : integer
     { signed = false,
     byte_size = 8 }

   IGNORE_ : string
     { byte_size = 12 }

   extras : string
     { byte_size = extraslen }

   key : string
     { byte_size = keylen }

   values : string
     { byte_size = bodylen - extraslen - keylen }
