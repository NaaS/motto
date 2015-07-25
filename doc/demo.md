# Quick Flick demo

In this demo we will build a runtime that will set up some context, including
a channel, load a file containing the [factorial function in
Flick](https://github.com/NaaS/system/blob/master/crisp/tests/factorial.cp),
then apply that function to produce a result.

First, take a look at this [example runtime script](https://github.com/NaaS/system/blob/master/crisp/runtime_factorial.ml).
Each line is documented.

Now build it using the following command, which you need to run in the directory
containing Motto:
```
$ ./build.sh runtime_factorial.byte
```
This will generate OCaml bytecode containing the runtime script seen earlier
(including any Flick code contained within).

Run the bytecode:
```
$ ./runtime_factorial.byte
```

And you'll get this output:
```
let y = 13 ~> 13
let z = 5 + (y * ?somechan) + y ~> 70
factorial (z - (60 + ?somechan)) ~> 40320
```

The output consists of each "Eval" line in the runtime script, followed by "~>",
and followed by the value to which the line evaluated.
