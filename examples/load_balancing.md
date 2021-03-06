# HTTP load balancers

In this note i attempt to sketch some of the design space for an HTTP-level load balancer
(HLB), a form of Layer 7 load balancer, a form of reverse proxy.
Then i seek to sketch a candidate implementation of such a thing using the
[crisp](https://github.com/NaaS/admin/wiki/crisp) syntax.

As an aside, performance-wise it would be useful (though perhaps very
ambitious) to compare with systems like
[HAProxy](http://www.haproxy.org/#plat) -- reported to have a forwarding rate up
to 40 Gbps -- and Nginx.
Willy Tarreau has some load-balancer performance test [advice](http://www.haproxy.org/#perf).

The parts to an HLB's design include:
* Scheduling function: how to balance the load -- i.e., how to pick/schedule a backend. There are
  various approaches to this.

  * Load insensitive: A feature of the request is used to determine which backend
    server to route to, without regard to the backend's load.
    This can be of various forms.

   * URL dependent -- this is the approach used in how we've been discussing
     HTTP load balancers. An example use-case consists of a [caching
     proxy](http://blog.octo.com/en/http-caching-with-nginx-and-memcached/).

   * Client-feature dependent:

     * [nginx](http://nginx.org/en/docs/http/load_balancing.html) supports"ip-hash : a
       hash-function is used to determine what server should be selected for the
       next request (based on the client’s IP address)".
       This policy is similar to that used in
       [Azure](http://azure.microsoft.com/blog/2014/04/08/microsoft-azure-load-balancing-services/).
       I'm not sure how this compares to Amazon's
       [ELB](https://aws.amazon.com/elasticloadbalancing/), but both Azure and ELB
       seem to be Layer 4, not Layer 7, load balancing; in fact
       [Snapt](http://www.snapt.net/products/balancer/ec2) capitalises on this.

     * Cookies: these are used by various implementations, such as
       [Snapt](http://www.snapt.net/products/balancer/features), which uses it for
       both HTTP and RDP, mainly to maintain session invariants.

     * Header dependent. For instance, HAProxy allows specifying [regexes over
       headers](http://www.haproxy.org/#secu).

   * Round-robin (supported by virtually all load balancers), and weighted
     variants of this. When not used in combination with other scheduling
     parameters, round-robin scheduling cannot guarantee stickiness/persistence
     of the routing between the client and backend.

  * Load/availability sensitive: the scheduling decision considers properties of
    the backend, rather than (exclusively) of the client or their request. For instance, one might wish to
    pick the backend that is currently serving the *fewest connections*, or that
    currently has the *least response time*.

    Actively polling the backends for their load information can be too
    expensive. Instead, many implementations seem to rely on 

    * _inferring availability information from backend replies_.
      This is feasible since the replies are going through the
      proxy. Existing implementations use the backend's response time,
      and HTTP status codes, to infer backend health.

    * _estimating availability based on the requests forwarded to the backend_.
      This is feasible since requests are going through the proxy.
      Here the proxy could count the
      number, and type, of requests being sent to a backend, in order to
      estimate the load being placed on the backend.

    In both cases, the proxy would maintain an index of backends and their
    metrics. This is then used to schedule connections to the backends.

    Apache's [mod_proxy_balancer](https://httpd.apache.org/docs/2.2/mod/mod_proxy_balancer.html)
    supports three such methods (it doesn't seem to support non-sensitive ones):
    "Request Counting, Weighted Traffic Counting and Pending Request Counting".
    The first and third focus on the number of requests, and the second on the
    traffic, as metrics for load to be balanced. The first two schedule load on
    the backends maximal in the difference between their current and expected
    loads; the third schedules to the least busy server (in terms of how many
    connections it is serving).

    [nginx](http://nginx.org/en/docs/http/load_balancing.html) supports
    "least connected: next request is assigned to the server with the least
     number of active connections"

* How to route to the backend server once this has been picked.
  Choices include:

  * Return a response to the client using HTTP code 307 to redirect it to the
    backend server. The client will make an identical request to the backend,
    directly. There are various reasons why this choice is not suitable to our
    work; some of these reasons will emerge in the description below.

  * Proxy between client and backend for the duration of the request.
    This is less scalable than first approach, since the middlebox
    becomes a bottleneck. In some cases (e.g., firewall) this is unavoidable,
    but HTTP load-balancing doesn't feel like it is necessarily such a case.

    On the other hand, this isolation provides increased security, and also
    means that the backends do not need to have public addresses.

    This also means that it's easier to know the number of active sessions,
    since the load-balancer can be used to monitor such metrics.

    We can use something comparable to semaphores to limit the number of active sessions?

* I think we ignored monitoring and analysis in the NaaS discussion so far, but these are cited as important
  features by implementations such as
  [Snapt](http://www.snapt.net/products/balancer/features).
  This can be used for admins to understand their network better.

* Such analyses are also tied to handling events such as timeouts or backend errors.
  For example, Snapt measures server response times and scans for errors, and
  can produce alert events based on this (to notify an SRE, presumably).

  Nginx uses this information a "passive health check", and Nginx will avoid
  scheduling jobs to servers that seem unhealthy. There are parameters like `max_fails` and
  `fail_timeout` that informs this too.

  Perhaps automated action could be taken in the event of:
  * a drop in the number of available servers
  * one server seems to fail
  * one server seems to take too long
  * average reponse rate rising above a threshold

* Several implementations provide *extensions* to proxy behaviour, of various
  kinds.

  * Protocol translation is provided by systems like 
    [Snapt](http://www.snapt.net/products/balancer/features),
    which translates from IPv6 (between client and proxy) to IPv4 (between proxy and backends).

  * Injecting identifiers at the proxy, in the form of cookies or URL-encoded state.
    (This idea will be used in the design sketched later.)

  * Integration with ACL or other form of access control.

  * [Filtering](http://www.haproxy.org/#secu) requests.

  * Caching, relying on header info, such as the "Cache-control" field (cf RFC 2616).

  * Freeing-up backend servers from transfers to slow clients.

  * A/B testing.


## Persistence
Should we assume that a client will speak to the same server for as long as the
session lasts? That is, does the routing need to be
_sticky_? This is standard behaviour in
[mod_proxy_balancer](https://httpd.apache.org/docs/2.2/mod/mod_proxy_balancer.html)
and [HAProxy](http://www.haproxy.org/). Since [snapt](http://www.snapt.net/)
commercialises HAProxy, presumably it has this behaviour too. This is *not* the
default behaviour in Nginx, which uses round-robin by default.
Stickiness can be enforced using URL encoding or by cookies, both of which are
administered by the proxy.


## Other protocols
In addition to HTTP and HTTPS,
[Nginx](http://nginx.com/resources/admin-guide/load-balancer/) supports
"FastCGI, SCGI, uWSGI and memcached".
[Snapt](www.snapt.net) can also load balance "SMTP, RDP, SQL and more".


# Sketch of an improved HLB use-case
Now we turn to sketching a more sophisticated HTTP load-balancer than what we
considered so far in NaaS discussions.

These are the processes in this sketch, they will be described in more detail
later.

1. the *load monitor*, which tracks one or more metrics related to each backend
   server --- for instance, the number of active connections on each
   individual backend server.

2. the *load balancer*, which consults the load monitor to find a minimally
   loaded server when a new connection comes in, and then sends the
   connection on to that backend. It registers this with the monitor, which
   updates its load estimate. When the connection terminates, the monitor is
   informed about this too.

The processes have different durations: the load balancer processes lives for as
long as the client's connection lives, while the monitor connection needs
to be persistent.

This also means that the monitor is a single point of failure. In order to
improve availability in this design, we don't need to replicate the load
balancer, but should replicate the load monitor. Replication can consist of
having more than one instance of the monitor running, each connected to the
channels to which the main monitor is connected to. Then if the main monitor
dies, one of the replicants(!) can take over. (I haven't thought of the
mechanics of this yet.)

The processes are connected via a channel of some description. Channels
abstracted in crisp --- they could be a TCP connection, or some other
reliable ordered medium, such as a pipe. The precise details of the
channel do not matter --- this resonates with the I/O abstraction work that
Anil and mort had proposed, based on
[Fable](http://anil.recoil.org/papers/drafts/2012-resolve-draft1.pdf).


## Application-level processing

So far, this design sketch doesn't include any application-level processing. This will
be added next.

We will create, examine, and update cookies to bind sessions to
specific backends. The client will continue communicating to the same backend
(through the proxy) for the duration of a session.

We now look at the *load balancer* process, which behaves as follows:

1. When a client request arrives, check for a valid cookie indicating that it
   has previously been bound to a backend.

2. If this exists, then use it to route to that backend.

  1. If the backend is unavailable, then

     1. Assign a new backend (by consulting the *load monitor*)

     2. Update the client's cookie when relaying the backend's response to the
        client.

  2. Otherwise route the request to that backend, leaving the client's cookie
     unchanged when relaying the backend's response to the client.

3. Otherwise, consult the load monitor to assign a backend to this client.

   1. Forward the client's request to the backend

   2. Encode the choice of backend in a cookie that gets sent to the client
      together with the backend's response.

Slightly more formally (a revised snippet will be given later):
```
proc Http_load_balancer : (http_request/http_reply client,
                           int/- next_srv,
                           [http_reply/http_request] backends)
  let input = client?
    # Assuming associative arrays, which can be encoded as lists of pairs
    if defined(input.cookie["backend_info"]) &&
        defined(input.cookie["backend_expiry"]):
        # FIXME Decode info and expiry
        # FIXME Check expiry
        server[input.cookie["backend"]] ! input
      else:
        # next_srv is a channel that contains an ordering for balancing load
        # among servers. if it is a queue, it could end up becoming unfair. so
        # instead, it should be a queue that's backed by a process -- the load monitor.
        let backend_id = next_srv?
          backends[backend_id] ! input
            let response = backends[backend_id]?
              # FIXME could encode backend_info
              response.cookie["backend_info"] := backend_id
                # FIXME could encode backend_expiry
                # FIXME set backend_expiry
                  client ! response
```

A problem with this design is that it checks the backend cookie upon the receipt
of each request. Instead, we could check this at the start of the connection
and use the same backend for the duration of the connection (for all requests
therein), and set the cookie in case the client connects again soon.

The two different designs also reveal two different ways in which the load
balancer interacts with the load monitor. In the first way, we could achieve
a much finer granularity --- informing the monitor each time a backend has
started and finished serving a request. It's not clear if this level of detail
is useful. I think that it isn't.
Using the second approach we have a coarser granularity of state changes ---
informing the monitor each time a backend has been connected and disconnected
with a client.

We can revise the above snippet to modify its behaviour in this regard, and add
more details:
```
# Alternates between sending a client request to a backend, and sending the
# backend's response to the client.
# NOTE does this forever, but in practice will be killed when either channel
#      is closed.
# NOTE this code doesn't do any application-level processing, so it can be
#      heavily optimised during compilation.
proc To_and_fro : (http_request/http_reply client, http_reply/http_request backend)
  repeat 1 instance forever
    backend ! client?
      client ! backend?

# Used to communicate activity updates from load balancers to the load monitor.
# Updates indicate whether a backend has been forwarded a connection, or if a
# previously forwarded connection has been closed. This allows the load monitor
# to monitor the "load" on each backend, estimated using a "number of
# connections" metric.
type activity : record
  backend : int
  activity : variant
    client_inc : unit
    client_dec : unit

proc Http_load_balancer : (http_request/http_reply client,
                           int/unit next_srv,
                           -/activity monitor_updates,
                           [http_reply/http_request] backends)
  let input = client?
    # Assuming associative arrays, which can be encoded as lists of pairs.
    if defined(input.cookie["backend_info"]) &&
        defined(input.cookie["backend_expiry"]):
        # FIXME Decode info and expiry
        # FIXME Check expiry
        let backend = server[input.cookie["backend"]]
          monitor_updates ! {backend := backend, activity := client_inc}
            backend ! input
              To_and_fro(client, backend)
            on_close(client) || on_close(backend)
              monitor_updates ! {backend := backend, activity := client_dec}
                exit
      else:
        # next_srv is a channel that contains an ordering among backends, for balancing load
        # among backends. if it is a queue, it could end up becoming unfair
        # (unless the queue is reordered when new info comes in, which makes it
        # NOT a queue). So instead it should be a queue that's backed by a
        # process -- the load monitor.
        # We request information from the load monitor by sending unity down the
        # channel.
        next_srv ! <>
          let backend_id = next_srv?
            let backend = backends[backend_id] # 'backend' is a channel alias.
              # NOTE technically we require a unity value following client_inc,
              #      but since we don't have HO functions i think we can safely insert
              #      such values in this context (i.e., when specifying which arm
              #      of a variant to use).
              monitor_updates ! {backend := backend, activity := client_inc}
                backend ! input
                  let response = backend?
                    # FIXME could encode backend_info
                    response.cookie["backend_info"] := backend_id
                      # FIXME could encode backend_expiry
                      # FIXME set backend_expiry
                        client ! response
                          To_and_fro(client, backend)
            on_close(client) || on_close(backend)
              monitor_updates ! {backend := backend, activity := client_dec}
                exit
```

## Load monitor
The *load monitor* does not process the data exchanged between the client or
backend; that is the task of the *load balancer* process.

The logic of the monitor is as follows: we continuously listen for monitor_updates,
and we use these values to update our scheduling (continuously). In parallel to
this, we listen for next_srv requests, to which we reply with a scheduling
decision based on the latest estimate of how loaded each backend is.
```
# A list of integers. Each element of the list indicates how loaded each server
# is, where bigger numbers mean bigger load.
type cfg : list int

# FIXME could specify invariant that load is never negative, and that it never
#       exceeds a certain positive value?

# The load monitor's behaviour is complementory to that of the load balancer(s).
# The monitor receives updates from balancers, and provides them with a
# schedule.
proc Http_load_monitor : (unit/int next_srv,
                          activity/- monitor_updates)
  let max_conns = 3000 # FUDGE maximum number of connections per backend.
  # FIXME currently we're not using max_conns anywhere.

  channel config : cfg/cfg # This channel is used to simulate state, by
                           # circulating a configuration value.
    cfg ! ... # FIXME initialise a vector of 0s.
      repeat 1 instance forever
        # The two blocks below are run in parallel, forever.
        # One updates the state, and the other reads it; there's no data race
        # for updating the state. We _might_ read a stale value of the state,
        # but in this case i'm assuming it's not a problem.
        # FIXME think more about consistency model.
        let update = monitor_updates?
          case update.activity of
            client_inc ->
              let config = cfg?
                let config' =
                    # This is the config list but where config'[update.backend] = config[update.backend] + 1
                    map_i i, load in config:
                      if i = update.backend:
                          load + 1
                        else: load
                  cfg ! config' # Recycle the updated state.
            client_dec -> # FIXME as with previous case, but decrementing.
        next_srv? # We don't need to let-bind with the unity value.
          let config = cfg?
            next srv !
                # Go through the list of servers and pick the one with the least load.
                fold b, current_best in config, MASSIVE_NUMBER: #FIXME need a special symbol for this
                  if b < current_best then b else current_best
              cfg ! config # Recycle the state.

# FIXME the "recycle" the state idiom seems clunky -- seems to be a good
#       candidate for sharper syntax.
```


## Bringing the pieces together
We connect the balancer and monitor together below.
This is *not* strictly a program in our language, since `client` must not yet have been fixed.
That is, the channel intialisation semantics is slightly different than what i
intended earlier.
Moreover, the `repeat` is intended to occur for different `client` channels, not the same
one.

Rather than using a crude `?` annotation to indicate this, could instead
iterate over an unbounded number of input channels -- we discover what they are
as they are created?
(Perhaps this would make channel semantics more consistent between "input"- and
"output"-style channels, since currently I'm assuming that output channels are
opened on demand, but that the input channel is already open when the Main
process starts.)

Some other consistency thoughts: channels may close, but they may not reopen. The identity
of each channel (i.e., the offset in the iteration) should not matter for
channel-addressing reasons? (i.e., during the iteration we could know that we're
in the 244th iteration, but not address channels numerically in case they're
closed? A better solution would be to use the primitive that tests for channels
being opened; testing a channel doesn't open it if it's currently not open.)
Also, we cannot evaluate `|client|` over such an (unbounded)
array of channels, because the value is not an integer.

```
proc Main : (http_request/http_response client?,  #FIXME note ? annotation.
             [http_reply/http_request] backends)
  channel monitor_update : activity/activity
  channel next_srv : unit | int # NOTE we don't want to specify the "direction"
                                # of this local channel since it will point
                                # from one process instance to another.
                                # (In fact, the channel will exist between
                                # all load balancers and the load monitor, as
                                # will the channel called monitor_update, but
                                # monitor_update will carry the same type in
                                # both directions, so it doesn't really matter
                                # which direction it's oriented in.
    Http_load_monitor (next_srv, monitor_updates)
      repeat 30000 instances forever
        # FIXME note the ? annotation below.
        Http_load_balancer (client?, next_srv, monitor_updates,  backends)
```

