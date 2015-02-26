## Main type definition

# Based on
#   https://github.com/NaaS/system/blob/master/naasferatu/front-end/hadoop*.naas
# More recent version of target code available at
#   https://lsds.doc.ic.ac.uk/gitlab/naas/naas-box-system/tree/master/src/applications/hadoop_data_model

type hadoop_wc : record
  # FIXME not sure what a "vlen" is in that example -- i can understand that
  #       it's a variable length integer, so shall we just call it an integer?
  key_len : integer
    { signed = false,
    endianness = big,
    # "size" in bytes
    size = 2 }
  key : string
    { size = "hadoop_wc.key_len" }
  value : integer
    { signed = false,
    endianness = big,
    size = 4 }
