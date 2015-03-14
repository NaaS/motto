include "hadoop_wc.cp"

# Returns true if all channels are able to provide an input.
fun AllReady : ([type hadoop_wc/-] chans) -> (boolean)
  for i in unordered chans
  initially acc = True:
    acc and not (peek(chans[i]) = None)

