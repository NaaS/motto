Motto is a tool that processes Flick programs:

* It compiles programs to run on a separate back-end runtime system.

* Provides a reference runtime for running Flick programs without requiring
  additional dependencies.

* Enables querying of Flick programs -- for instance, type-checking Flick
  expressions from the command-line.

**NOTE** Motto accepts several switches. At the moment most of them are
only described in the [code](https://github.com/NaaS/motto/blob/master/motto.ml).


# Building

To compile Motto:
```
$ ./build.sh
```

If compilation fails, check that you have all dependencies listed in the
"Dependencies" section below.

Try running `./motto.byte -h` to see if you have a binary. See the section on
"Testing" for how to run the tool over regression tests.


# Getting going
We start with the mathematical cousin of "Hello, world" -- i.e., the factorial
program. Look at the [factorial function in Flick](https://github.com/NaaS/motto/blob/master/tests/flick_code/factorial.ml).
To run this, we need an environment that can interpret it -- Motto provides
this. This interpreter requires a runtime, which connects it with resources on
the machine on which Flick programs are interpreted. For instance, sockets
aren't a concept that Flick provides; they are resources that the machine makes
available to the runtime, that in turn provides them to running Flick programs
as channels.

Motto provides a flexible scriptable-runtime. That is, in addition to running
Flick programs, you can programmatically control different aspects of the
runtime. This is useful, for instance, if you want to alter the contents of
channels, to make tests more repeatable and specific.  If you have the time,
it's not difficult to turn this into an _interactive_ runtime -- i.e., a
"toplevel" or Read-Eval-Print Loop, a.k.a., "REPL".)

Now look at an example of the [runtime's use to run the factorial function](https://github.com/NaaS/motto/blob/master/tests/runtime/factorial.ml).
Using the scriptable runtime, you can compile and run Flick programs using the
OCaml compiler and runtime.  To compile the example:
```
$ ./build.sh factorial.byte
```
and then run!
```
$ ./factorial.byte
```

You should get the following output:
```
let y = 13 ~> 13
let z = 5 + (y * ?somechan) + y ~> 70
factorial (z - (60 + ?somechan)) ~> 40320
```
This output reproduces each expression that was evaluated by the runtime,
followed by `~>`, and followed by the value that the expression evaluated to.

Tracing the evaluation within expressions is not difficult to implement --
essentially by adding and using a hook in the [evaluation monad](https://github.com/NaaS/motto/blob/master/runtime/eval_monad.ml) -- but currently
this is not implemented.

Take a look at this [example](https://github.com/NaaS/motto/blob/master/tests/runtime/runtime_general_test.ml)
for a comprehensive example of using the scriptable runtime.


# Compiler

To compile a file named `source_file` run the following command:
```
$ ./motto.byte -o output_directory source_file
```
where `output_directory` is the name of the directory (that doesn't yet
exist) that's intended to hold the compiler's output.
The compiler will create `output_directory` and deposit the output files there,
where they can be compiled and linked using a separate toolchain.

Other than the reference runtime system, described above, only a single back-end
runtime system is supported. Thus code is generated exclusively for this
runtime.


# Testing

For testing, I'm currently using the following as `source_file`:
* `examples/hadoop_wc_type.cp`
* `tests/flick_code/simple_function.cp`

To run the parser regression tests on both the `examples` and `tests` directories:
```
$ ./motto.byte -q --parser_test_dir examples --parser_test_dir tests/flick_code
```
Or you might wish to only test a single file:
```
./motto.byte -q --parser_test_file tests/flick_code/types_process.cp
```

To run type inference on expressions:
```
$ ./motto.byte -q --infer_type "(| E |)"
```
where `E` is an expression.

To run the type checker on regression tests, see the
[test_type_checker.sh](https://github.com/NaaS/motto/blob/master/scripts/test_type_checker.sh) script.


# Background

The Flick language implements the [crisp](https://github.com/NaaS/admin/wiki/crisp) language idea,
but simplified as described in the
[Flick](https://github.com/NaaS/system/tree/master/crisp/flick) description.


# Dependencies

Other than the OCaml compiler, this project has the following depedencies:
* ocamlfind. This in turn depends on:
  * m4
* menhir
