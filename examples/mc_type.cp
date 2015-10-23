# Luo's merge of the memcached data types we had specified in the paper.

type mc_command : { diffingo_record = "MemcachedDMData", diffingo_data = "memcached_compact::CompactMemcachedCommand" } record
#   magiccode : string
#     { byte_size = 1 }

#   opcode : string
   opcode : integer
#     { byte_size = 1 }

#   keylen : integer
#     { signed = false,
#     byte_size = 2 }
#
#   extraslen : integer
#     { signed = false,
#     byte_size = 1 }
#
#   _ : string
#     { byte_size = 3 }
#
#   bodylen : integer
#     { signed = false,
#     byte_size = 8 }
#
#   _ : string
#     { byte_size = 12 }
#
#   extras : string
#     { byte_size = extraslen }

   key : string
#     { byte_size = keylen }

#   values : string
#     { byte_size = ((bodylen - extraslen) - keylen) }
