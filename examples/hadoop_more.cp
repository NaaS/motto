#// Reference to what comes out of a channel
#HadoopDMDgoData * peek_6;
#peek_0 = NaasData::peek_channel<HadoopDMDgoData> (inputs[x_recv_index_0]);
#// ... and anything related to a channel
#NaasData::write_to_channel<HadoopDMDgoData> (peek_1, outputs[0], &size);
#NaasData::consume_channel<HadoopDMDgoData> (inputs[0], &size);
#
#// Actual data of what comes out of a record
#hadoop::HadoopRecord *v2= reinterpret_cast<hadoop::HadoopRecord *>(peek_1->area.contents());

type k_v : { diffingo_record = "HadoopDMDgoData", diffingo_data = "hadoop::HadoopRecord" } record
  key_len : integer
  key : string
  value_len : integer
  value : integer

process Wc_node : (k_v/- x, k_v/- y, -/k_v z)
  # Instead of "vs" could have pattern matching, to give v1 and v2
#  let vs = peek_all ([x, y]) # let [v1,v2] <= [x,y]
  let v1 : k_v = ??x
  let v2 : k_v = ??y

  if v1.key_len = -1 and v2.key_len = -1:
    x => z # This syntax means that we're "forwarding" a value from channel x
           # to channel y. The value is consumed from channel x.
    y => _ # This is used to discard the next item on channel y.
  else: if v1.key_len = -1:
    y => z
  else: if v2.key_len = -1:
    x => z

  else: if v1.key < v2.key:
    x => z
  else: if v2.key < v1.key:
    y => z
  else:
#    v1.value := v1.value + v2.value # Alternative syntax:  in-place update for record field
#    v1 => z # Overload "=>" syntax to have values (and not only channels) on
             # the LHS
    z ! (v1 with value = v1.value + v2.value)
    x; y => _ # * discarding next items from multiple channels
