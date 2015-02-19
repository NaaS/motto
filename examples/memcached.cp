
# FIXME update to match proper spec, and include serialisation annotations
type mcd_request : record
  key : string
type mcd_reply : record
  key : string
  value : string
#
proc MCD : (type mcd_request/type mcd_reply client, [type mcd_request/type mcd_reply] backends)
  # NOTE shared dictionaries are application-level primitives we supply.
  # FIXME give type of the dictionary
  # NOTE what parameters to give e.g., whether to randomise, and initial allocation.
  global cache := empty_dictionary
#
  # Any time we get something from a backend, cache it and forward it to the
  # client.
  backends => update_cache(cache) => client
#
  # client => backends
  let request = read(client)
  if cache[request.key] = None:
    # Work out which backend memcached to forward this request to, and send.
    request => backends[hash(request.key) mod len(backends)]
  else:
    cache[request.key] => client
#
fun update_cache : (cache : type pbr_BLA, response : type mcd_reply) -> (type mcd_reply)
# FIXME relying on pass-by-reference of cache
# FIXME need array assignment syntax
#  cache[response.key] := response
  response

