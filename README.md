Implementation of the [crisp](https://github.com/NaaS/admin/wiki/crisp) language idea.

To compile: `./build.sh`

To run: `ocamlrun crisp_test.byte`

## TODO
* Which features can be skipped for initial implementation?
  Ideas: dependent types, variants.
* Parser+AST
* Passes (not necessarily in this order):
  * Type checking+inference, to detect malformed expressions.
  * Isolation analysis, to detect dead code (forming a loop that's disjoint from
    the rest of the graph).
  * Termination condition checks (the main process must have termination
    conditions associated with at least one channel?).
  * Inlining processes as much as possible, removing redundancies, then breaking
    up into N processes based on some optimisation criterion.
* Translate into C+libNAAS
