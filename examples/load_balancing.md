# HTTP load balancers

This note attempts to sketch some of the design space for an HTTP-level load balancer
(HLB), a form of Layer 7 load balancer, a form of reverse proxy.

Performance-wise, would be useful to compare with systems like
[HAProxy](http://www.haproxy.org/#plat) -- reported to have a forwarding rate up
to 40 Gbps -- and Nginx.
Willy Tarreau has some load-balancer performance test [advice](http://www.haproxy.org/#perf).

The parts to an HLB's design include:
* How to balance the load -- i.e., how to pick/schedule a backend. There are
  various approaches to this.

  * Load insensitive: A feature of the request is used to determine which backend
    server to route to, without regard to the backend's load.

   * URL dependent -- this is the approach used in how we've been discussing
     HTTP load balancers. An example use-case consists of a [caching
     proxy](http://blog.octo.com/en/http-caching-with-nginx-and-memcached/).

   * Client-feature dependent:

     * [nginx](http://nginx.org/en/docs/http/load_balancing.html): "ip-hash : a
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

   * Round-robin (supported by virtually all load balancers)

  * Load/availability sensitive: the scheduling decision considers properties of
    the backend, rather than of the client or their request. One might wish to
    pick the backend that is currently serving the *fewest connections*, or that
    currently has the *least response time*.

    Actively polling the backends for their load information can be too
    expensive. Instead, many methods seem to rely on 

    * _inferring this information from backend replies_ since the replies are going through the
      proxy). Here we could use the response time, and HTTP status codes, to infer
      backend health.

    * _estimating this based on the requests forwarded to the backend_ (since
      requests too are going through the proxy). Here the proxy could cound the
      number, and type, of requests being sent to a backend.

    In both cases, the proxy would maintain an index of backends and their
    metrics. This is then used to schedule tasks on the metrics.

    [mod_proxy_balancer](https://httpd.apache.org/docs/2.2/mod/mod_proxy_balancer.html)
    supports three such methods (it doesn't seem to support non-sensitive ones):
    "Request Counting, Weighted Traffic Counting and Pending Request Counting".
    The first and third focus on the number of requests, and the second on the
    traffic, as metrics for load to be balanced. The first two schedule load on
    the backends maximal in the difference between their current and expected
    loads; the third schedules to the least busy server (in terms of how many
    connections it is serving).

    [nginx](http://nginx.org/en/docs/http/load_balancing.html):
    "least connected: next request is assigned to the server with the least
     number of active connections"

* How to route to the backend server once this has been picked.
  Choices include:

  * Return a response to the client using HTTP code 307 to redirect it to the
    backend server. The client will make an identical request to the backend,
    directly.

  * Proxy between client and backend for the duration of the request.
    This is less scalable than first approach, since the middlebox
    becomes a bottleneck. In some cases (e.g., firewall) this is unavoidable,
    but HTTP load-balancing doesn't feel like it is necessarily such a case.

    * On the other hand, this isolation provides increased security, and also
      means that the backends do not need to have public addresses.

    * This also means that it's easier to know the number of active sessions,
      since the load-balancer can be used to monitor such metrics.

    * We can use something comparable to semaphores to limit the number of active sessions?

* I think we ignored monitoring and analysis in the NaaS discussion so far, but these are cited as important
  features by implementations such as
  [Snapt](http://www.snapt.net/products/balancer/features).
  This can be used for admins to understand their network betterm.

* Such analyses are also tied to handling events such as timeouts or backend errors.
  For example, Snapt measures server response times and scans for errors, and
  can produce alert events based on this (to notify an SRE, presumably).

  Nginx uses this information a "passive health check", and Nginx will avoid
  scheduling jobs to servers that seem unhealthy. There are parameters like `max_fails` and
  `fail_timeout` that informs this too.

  Perhaps automated action could be taken in the event of:
  * number of available servers drops
  * one server seems to fail
  * one server seems to take too long
  * average reponse rate rise is above a threshold

* Several implementations provide *extensions* to proxy behaviour.

  * Protocol translation is provided by systems like 
    [Snapt](http://www.snapt.net/products/balancer/features),
    which translates from IPv6 (between client and proxy) to IPv4 (between proxy and backends).

  * Injecting identifiers at the proxy, in the form of cookies or URL-encoded state.

  * Integration with ACL or other form of access control.

  * [Filtering](http://www.haproxy.org/#secu) requests.

  * Caching, relying on header info, such as "Cache-Control" header (cf RFC 2616).

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
[Snapt](www.snapt.net) can also load balance "SMTP, RDP, SQL and more".
In addition to HTTP and HTTPS,
[Nginx](http://nginx.com/resources/admin-guide/load-balancer/) supports
"FastCGI, SCGI, uWSGI and memcached".


