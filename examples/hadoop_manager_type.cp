# Attempt at encoding the type for Hadoop Manager data.

type hadoop_manager : record
  # NOTE have a nested record called 'header'.
  # FIXME should the header be 27 bytes long? Is that a constant?
  header : record
    # Ignore the first 4 bytes
    _ : string
      { byte_size = 4 }

    job_id : integer
      { signed = true,
      encoding = ascii,
      byte_size = 8}

    # Ignore the following 10 bytes
    _ : string
      { byte_size = 4 }

    reducer_id : integer
      { signed = true,
      encoding = ascii,
      byte_size = 8}

    job_name_len : integer
      { signed = true,
      # FIXME should job_name_len be a (hadoop) vint?
      encoding = ascii,
      byte_size = 1}

  # NOTE 'job_name' is not included in the 'header' record.
  job_name : string
