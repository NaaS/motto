type channel_metadata : record
  interface : string
  src_network_address : ipv4_address
  dst_network_address : ipv4_address
  protocol : variant
    TCP : record
      src_port : integer
      dst_port : integer
# NOTE currently we're ignoring UDP
#    UDP : record
#      src_port : integer
#      dst_port : integer

