include "mc_type.cp"

proc MCD : (type mcd_request/type mcd_reply client, [type mcd_request/type mcd_reply] backends)
  # NOTE (possibly shared) dictionaries are application-level primitives we supply.
  # FIXME specift what parameters to give? e.g., whether to randomise, and initial allocation.
  global cache : dictionary <string * string> := empty_dictionary

  # Any time we get something from a backend, cache it and forward it to the
  # client.
  backends => update_cache(cache) => client

  client => test_cache_or_pass_on(client, backends)

fun update_cache : (cache : ref dictionary <string * string>, response : type mcd_reply) -> (type mcd_reply)
  # NOTE relying on pass-by-reference of cache
  cache[response.key] := response
  response

fun test_cache_or_pass_on : (type mcd_request/type mcd_reply client, [type mcd_request/type mcd_reply] backend; request : type mcd_request) -> ()
  if cache[request.key] = None:
    # Work out which backend memcached to forward this request to, and send.
    request => backends[hash(request.key) mod len(backends)]
  else:
    cache[request.key] => client
