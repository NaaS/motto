Implementation of the [crisp](https://github.com/NaaS/admin/wiki/crisp) language idea,
but simplified as described in
[Flick](https://github.com/NaaS/system/tree/master/crisp/flick).

# Flick in funf minuten
Take a look at this [example](https://github.com/NaaS/system/blob/master/crisp/runtime_test.ml)
of using the scriptable runtime. (If you have the time, it's not difficult to
turn this into an _interactive_ runtime -- i.e., a "toplevel" or Read-Eval-Print Loop, a.k.a., "REPL".)

Using the scriptable runtime, you can compile and run Flick programs using the
OCaml compiler and runtime.  To compile the example:
```
$ ./build.sh runtime_test.byte
```
and then run!
```
$ ./runtime_test.byte
```

The full list of commands supported by the scriptable runtime is currently only
documented in the [code](https://github.com/NaaS/system/blob/master/crisp/runtime_inspect.ml)


# Compiler

To compile the compiler:
```
$ ./build.sh
```

To run:
```
$ ./otto.byte -o output_directory source_file
```
where `output_directory` is the name of the directory (that doesn't yet
exist) that's intended to hold the compiler's output.
The compiler will create `output_directory` and deposit the output files there,
where they can be compiled and linked using a C++ toolchain.

**NOTE** the compiler accepts several switches. At the moment all of them are
only described in the [code](https://github.com/NaaS/system/blob/master/crisp/otto.ml).


# Testing

For testing, I'm currently using the following as `source_file`:
* `examples/hadoop_wc_type.cp`
* `tests/simple_function.cp`

To run the parser regression tests on both the `examples` and `tests` directories:
```
$ ./otto.byte -q --parser_test_dir examples --parser_test_dir tests
```
Or you might wish to only test a single file:
```
./otto.byte -q --parser_test_file tests/types_process.cp
```

To run type inference on expressions:
```
$ ./otto.byte -q --infer_type "(| E |)"
```
where `E` is an expression.

To run the type checker on regression tests, see the [test_type_checker.sh](https://github.com/NaaS/system/blob/master/crisp/scripts/test_type_checker.sh) script.
