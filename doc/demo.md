# Quick Flick demo

First, take a look at this [example runtime script](https://github.com/NaaS/system/blob/master/crisp/runtime_factorial.ml).
Each line is documented.

Now build it using the following command, which you need to run in the directory
containing Otto:
```
$ ./build.sh runtime_factorial.byte
```

Then run it:
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
