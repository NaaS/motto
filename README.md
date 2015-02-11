Implementation of the [crisp](https://github.com/NaaS/admin/wiki/crisp) language idea,
but simplified as described in
[Flick](https://github.com/NaaS/system/tree/master/crisp/flick).

To compile: `./build.sh`

To run: `ocamlrun crisp_test.byte`

## TODO
* Parser+AST
* Passes (not necessarily in this order):
  * Type checking+inference, to detect malformed expressions.
  * Dead code
  * Busy waiting (e.g., AllReady-style behaviour)
  * Inlining processes as much as possible, removing redundancies, then breaking
    up into N processes based on some optimisation criterion.
* Translate into C+libNAAS
