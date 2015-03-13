# Luo's merge of the memcached data types we had specified in the paper.

type mc_command
   magiccode : string
     { byte_size = 1 }

   opcode : string
     { byte_size = 1 }

   keylen : integer
     { signed = false,
     byte_size = 2 }

   extraslen : integer
     { signed = false,
     byte_size = 1 }

   _ : string
     { byte_size = 3 }

   bodylen : integer
     { signed = false,
     byte_size = 8 }

   _ : string
     { byte_size = 12 }

   extras : string
     { byte_size = extraslen }

   key : string
     { byte_size = keylen }

   values : string
     # FIXME arithmetic on field sizes isn't supported yet.
     { byte_size = "bodylen - extraslen - keylen" }
