## Main type definition

# Based on
#   https://github.com/NaaS/system/blob/master/naasferatu/front-end/hadoop*.naas
# More recent version of target code available at
#   https://lsds.doc.ic.ac.uk/gitlab/naas/naas-box-system/tree/master/src/applications/hadoop_data_model

type hadoop_wc : record
  key_len : integer
    { signed = true,
    # FIXME not sure if the target allows us to specify endianness at present.
    #endianness = big,
    # "size" in bytes
    byte_size = 2 }
  key : string
    { byte_size = key_len }
  value : integer
    { signed = false,
    #endianness = big,
    byte_size = 8 }
