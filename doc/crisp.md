Process could return a value when they terminate?

# Motivation
In the NaaS project we sketch a general "in-network program" (INP) as a square with _m_ incoming lines (channels) on the left, and _n_ outgoing channels on the right. This is used as a generalisation of any INP that a programmer can write to run on our system. Thus the abstraction we're using is that of a "box with channels", where data flows over the channels to and from the box, and the behaviour of the program (box) will range from processing a single item of input, to running continuously for as long as there is data coming in on the input channels.

Our sketches suggests two ideas:
* INPs cannot satisfactorily be described as functions: they don't map something to something else, in a "local" sense. They can be seen as mappings in a "global" sense -- that is, they map the world into another world, transforming its state -- for instance, by moving data between buffers. This is far too weak an abstraction to be used as a sensible typing specification -- all functions would be typed `World -> World`.
* Moreover, INPs can have values flowing in the other direction on the channels. This was not apparent from our initial sketches, but arose later on. Again, if we were to model INPs as functions we're in for a vague time (`World -> World`) or we need a better abstraction.

The abstraction we use here matters, since it'll shape the language we could use to describe INPs. Writing functions typed `World -> World` is of no use to anyone. Writing functions typed `InputType -> OutputType` is wrong since it wouldn't distinguish between INPs that send replies back to the client, and those that don't.

## Processes
Instead of functions, a better abstraction would consist of _processes_ that communicate over _channels_. Compared to functions, processes are much closer to the intuition we have of INPs. Processes are a special case of functions. They are functions that are better tailored for the kind of programs we want to express. We can encode function-like behaviour (i.e., maps) using processes, as will be described later.

To start with, it's important to note the following isomorphism. The general INP
```
     |-------|
---->|       |---->
 m   |   f   | n
---->|       |---->
     |-------|
```
can equivalently be drawn as
```
     |-------|
---->|       |
 m   |       |
---->|   f   |
<----|       |
 n   |       |
<----|       |
     |-------|
```
Semantically it doesn't matter which one of these we use. I prefer using the second form since it canonicalises which channels are primarily going _towards_ and _away_ from the process we are defining. Note that channels carrying values _towards_ a process can still carry values in the opposite direction, but these second values are either the consequences of the first values (e.g., replies to requests that had been carried towards the process), or are conceptually subordinate to the first values (otherwise we should split that channel into two unidirectional channels -- one pointing to, and the other away, from the source). Note also that we should be able to specialise channels to be unidirectional -- this restriction can be used for compile-time checking of INPs.

I will use the notation `T1/T2` for a channel that provides values of type `T1` and accepts values of type `T2`. Using the second diagram form makes it easier to write the types of all channels, because they're all evaluated on the same "side" of the process -- not that sides have any topological meaning in this case. I will use the symbol `-` to mean the empty type. We cannot form values of the empty type. Thus the channel `int/-` is a channel that can provide values of type `int` but that cannot accept any values (of any type) -- it is a unidirectional channel. Similarly, the channel `-/int` is a channel on which we can only send values of type `int`; it cannot be queried for any values. Any program that did so would be malformed, and this would be reported at compile time.

Some more remarks before turning to a brief syntax primer, followed by examples in the proposed language. The language will be formalised later.
* Processing, input and output can happen concurrently. Some parts of the program might have to take place sequentially -- such as "assignment". (There are no variables in this language, so what I mean by assignment here consists of binding an identifier to a value. That identifier subsequently behaves as a scoped constant.)
* Processes are like blocks, and we can form them by combining existing blocks -- by connecting their channels together.

## Syntax primer
Processes are expressed using the following notation:
```
def <PROCESS_NAME> : (<TYPED CHANNELS>)
  <BLOCK 1>
  ...
  <BLOCK n>
```
where all blocks are executed in parallel.
A block consists of a guard and a (possibly empty) subordinate block. The evaluation of the subordinate block begins only when the guard is satisfied. The guard may modify the scope of the subordinate block. 

For example in the program below, the line `let x = input?` serves both as a guard for its subordinate block -- making the execution of the latter subsequent to a value becoming available on channel `input` -- and binds `x` to that value. The scope of `x` extends to the end of the block. This is indicated lexically -- by means of indentation.
```
def Test : (int/- input, -/bool output1, -/bool output2)
  let x = input?
    if x % 2 = 0:
        output1! true
        output2! false
      else:
        output1! false
        output2! true
```
*Channel communication* is a crucial part of this language, and is carried out via two operators:
* `?` is a postfix operator on channels that retrieves an item from that channel. If `c` is a channel, and that channel has type at least `T/-` for some type `T`, then the expression `c?` evaluates to a value of some type `T`. Its evaluation is blocked until such a value is available.
* `!` is an infix binary operator applied to a channel (of some type at least `-/T`) and a value (of type `T`). It evaluates to the unit type, at which point the given value has been sent on the given channel. The expression's evaluation is blocked until the value is sent on the channel.

The above program could be written equivalently as follows:
```
def Test : (int/- input, -/bool output1, -/bool output2)
  let x = (input? % 2 = 0)
    output1! x
    output2! ~x
```
where `~` is the negation symbol.

Blocks within a process are executed when their guards are satisfied. Guards are satisfied when their expressions have been evaluated. For example, this might occur when channel operations -- reading or writing -- succeed.

Processes can be stopped externally (by the scheduler, interpreting a policy). A process can also stop itself. This is useful for defining how processes are to behave in the presence of _failure modes_. In the modified example below, we are catching events related to channels. Specifically, if any of the process' channels are closed (for whatever reason), then the process terminates.
```
def Test : (int/- input, -/bool output1, -/bool output2)
  let x = (input? % 2 = 0)
    output1! x
    output2! ~x
  is_closed(input) || is_closed(output1) || is_closed(output2)
    exit
```
The `||` operator is a parallel-or operator.
The `exit` expression instructs the scheduler to remove this process (and any processes it invokes). **FIXME: not yet happy with semantics of this.**


# Examples
### HTTP load-balancer
Let's assume that the types `http_request` and `http_reply` have been defined, following RFC2616 say.

Our load balancer is specified to accept a single request from the client, forward it to a single backend server, and then relay the server's response back to the client. It does not accept a second request from the client.
```
// Accept a single request, forwarded to a single backend. Then relay the backend's (single) reply back to the client.
def Load_balance : (http_request/http_reply client, [http_reply/http_request] servers)
  let input = client?
    let target = hash(input.url) % |servers|
      servers[target] ! input
        client ! servers[target]?
```

What if we want to run this process continuously, to handle all the requests coming in from the client? This could be modelled by a omega-sequence of invocations of this processes. In practice this sequence will be finite -- it will end at the last request received from the client.
```
def Main : (http_request/http_reply client, [http_reply/http_request] servers)
  repeat 1 instance forever
    Load_balance(client, servers)
  on_close(client) || on_close(servers) //NOTE: lifted to array of channels
    exit
```
The new notation above notation means that after an instance of `Load_balance` terminates, we are to start another, then another, then another... but at any one time there must be at most one instance active. (This will ensure that the replies will have the same order as the requests. This would not necessarily be the case if we were to have more than one instance of `Load_balance` running.)

### Diode
```
def Diode : (T/- a, -/T b)
  b! a?
```

### Identity
```
def Id : (T1/T2 a, T2/T1 b)
  // The order of these two expressions doesn't matter: they are evaluated in parallel.
  b! a?
  a! b?
```

We could define an identity function as a composition of 2 diodes, if we allow subtyping between channels.
```
def Id2 : (T1/T2 a, T2/T1 b)
  Diode(a, b)
  Diode(b, a)
```
This seems sensible: we can treat `T1/T2` as `T1/-` or `-/T2`, but not vice versa.
That is, given a certain capability we can restrict the capability and use it in a context that requires that restricted capability, but we cannot go in the opposite direction. Channel subtyping will be described in more detail later.

### Mux
There are several kinds we could define (all sending stuff out on one channel):
* from 2 channels
* from n channels
* from n channels making lists of the n heads (1 from each channel)

Simplest (2 channels in, 1 channel out):
```
def Mux : (T1/- a, T1/- b, -/T1 c)
  c! a?
  c! b?
```

Note that the following are justified:
* Mapping an array of channels (carrying value of type `T`) into list (of values of type `T`). The inverse is not generally possible (since a list has variable size, but channel array doesn't -- this is unless we fix list size using dependent types, turning the list into a vector. This is described later on.)
* Treating a channel `-/[T] a` as `-/T b` (by implicitly folding over `?a` and sending each value down `b`), and also the inverse (by making `b` into a singleton list).
**FIXME: Say more about transient nature of lists; it may be that we can do without primitive lists, and have them as a defined notion instead.**

More sophisticated (_n_ channels in, 1 channel out):
```
def Mux_n_serial : ([T1/-] a, -/T1 b)
  for chan in a:
    b! chan?
```
Note that `for` can be defined in terms of `fold`. But this isn't quite the right semantic if we don't want to iterate through channels. Instead we could use a parallel fold, which gives rise to a parallel for. Parallel fold will be described later.
```
def Mux_n : ([T1/-] a, -/T1 b)
  for_p chan in a:
    b! chan?
```

### Demux
Inverting, as it were, `Mux_n`, gives us a round-robin demultiplexer:
```
def Demux_n_roundrobin : ([-/T] a, T/- b)
  for_p chan in a:
    chan! b?
```
We can get a different sort of demux if we fix the binding of values coming from channel `b`:
```
def Demux_n : ([-/T] a, T/- b)
  let x = b? in
    for_p chan in a:
      chan! x
```
Alternatively we could map a list of values to an array of channels, as long as the two have the same size. This can be enforced using dependent types:
```
def Demux_n_dep : {size} => ([-/T]{size} a, [T]{size}/- b)
  let x = b? in
    for_p i in range(|size|): // NOTE: range can be defined in terms of fold.
      a[i] ! x[i]
```

### Aggregation: average
```
def AggAverage : ([[int]/-] input, [int/-] output)
  let chans = input? //chans is of type [[int]] -- using abbreviation.
    output!
      // NOTE: map is definable in terms of fold.
      map (l : [int]) on chans: //NOTE: avoiding explicit lambda notation, as in fold. l is the abstracted variable of the function parameter, and chans is a parameter.
        let total =
          fold n, so_far in l, 0:
            n + so_far
        in total quot |l| //quot is the division operator on integers. FIXME.
```

### Aggregation: word count
**FIXME: this example can be made more legible by sugaring idioms, as shown in the Join example; this is totally fine as long as i ensure that sugar ultimately can be expressed in terms of first principles.***
```
type wc_key_val: record
  string key ("\n$")
  int val

type key_grouped_vals: record
  // NOTE: I used "skey" rather than "key" to have distinct constructors for different types.
  string skey // No need for end-matcher? Not having this is fine, but it would mean that the system will be unable to create a (de)serialiser. This is fine as long as this type doesn't appear as an input or output type in "Main".
  [int] vals // Decided to use Haskell-style list syntax, for more homogenous appearance (wrt channel arrays).

// NOTE: simulating parameter passing below (cf kv) -- this can be done using a local channel over which a single value is sent to this process at invocation, as shown in group_by
def Update : ([key_grouped_vals]/- input, wc_key_val kv, -/[key_grouped_vals] output)
  output !
    fold kvs, result_so_far in input?, []:
      if kvs.skey = kv.key:
          { skey = kvs.skey, vals = kv.val :: kvs.vals } :: result_so_far
        else
          kvs :: result_so_far

def Group_by : ([wc_key_val]/- input, -/[key_grouped_vals] output)
  output!
    fold kv, result_so_far in input?, []:
      keys_so_far = map(.key, result_so_far)
      if kv.key in keys_so_far:
          channel outp : [key_grouped_vals]/[key_grouped_vals]
            // Next 2 lines are the round-about way of doing parameter passing.
            channel r_s_f : [key_grouped_vals]/[key_grouped_vals]
                r_s_f ! result_so_far
                  Update(r_s_f, kv, outp)
                    outp? // round-about way of returning values.
        else:
          { skey = kv.key, vals = [kv.val] } :: result_so_far

def Main : {job_id, no_sources} => ([wc_key_val]{no_sources}/- keyvals, -/[wc_key_val] reducer)
  channel group_key_val : [key_grouped_vals]/[key_grouped_vals]
    Group_by (keyvals, group_key_val)
//  let group_key_val : key_grouped_vals = group_by(keyvals, .key) // NOTE could make group_by a built-in HOF, and have it take a projection function as parameter (i.e., .key in this case)
    let added_vals : [wc_key_val] =
      fold kvs, result_so_far in group_key_val, []
        { key = kvs.skey,
          val =
            fold v, sub_result_so_far in kvs.vals, 0
              v + sub_result_so_far
        } :: result_so_far
    reducer! added_vals
```

### Join
```
// Have 2 inputs, consisting of the serialisations of 2 tables.
def Join : ([(a, b1)]/- t1, [(a, b2)]/- t2, -/[(b1, b2)] t3)
  let table2 = t2?
  in
    t3!
      fold (x1, y1), _ in t1?, []:
        fold (x2, y2), so_far in table2, []: // we must use table2, not t2? here, otherwise we'd be pulling out a new version of t2 at each iteration of the outer fold.
          if x1 = x2:
            (y1, y2) :: so_far
          else
            so_far
```
Instead of requiring programmers to give definitions from first principles, as above, we could define short-cuts for idioms such as join. I think this is a good way forward: in principle, we could define all short-cuts/abbreviations from first principles, but in practice it's more helpful to provide abbreviations -- and have the implementation work from these abbreviations directly.

```
// Reworked example of a join query
def Join : ([(a, b1)]/- t1, [(a, b2)]/- t2, -/[(b1, b2)] t3)
  t3! inner join
    (x1, _) from t1?
    (x2, _) from t2?
    where x1 = x2
```

### Two simplexes give a duplex
```
def Ssd : (T1/- a, -/T2 b, T2/T1 c)
  c! a?
  b! c?
```
### Tap
```
def Tap : (T1/T2 a, T2/T1 b, -/T1 c_a, -/T2 c_b)
  let request = a?
    b! request
    c_a! request
  let reply = b?
    a! reply
    c_b! reply
```
### Source of 1s
Continuous stream of ones
```
def One : (-/int a)
  a! 1
def Ones : (-/int a)
  repeat 1 instance forever
    One(a)
```

Instead of having a continuous stream, we might prefer to emit values at certain intervals -- for instance, once every 50 milliseconds. If properly connected to a primitive, we could have a timer source as a channel:
```
def One' : (()/int timer, -/int a)
  timer! 50 // Send the number of milliseconds we want to wait.
    let _ = timer? // This step should block for 50 milliseconds.
      a! 1
```

A different idea would be to have a `sleep` primitive:
```
def One'' : (-/int a)
  sleep(50) // Blocks for 50 milliseconds.
    a! 1
```

### Emulating state
Connecting a channel to itself, can be done externally, as in the case shown next (assuming that `P` is a process that accepts two channels used for input and output of values). Here we emulate state by circulating outputs to inputs.
```
channel c : T1/T2
  repeat 1 instance forever
    P(c, c)
```

The above example is contrived. We could emulate state more convincingly by using local channels. (That is, `channel c` would occur _within_ `P` above.)

Note that it is possible to write nasty programs such as the following unless a "connectedness" test is carried out at compile time. This tests for isolated loops in the dataflow graph.
```
let Hell : ()
  channel c : ()/()
    c! ()
      repeat 1 instance forever
        c! c?
```

### Non-examples
* Channels crossover
```
def Crossover : (T1/T2 a, T1/T2 b)
  // We can't generally pull stuff from a and send it to b.
  // (This is only possible if `T2` is an upcast of `T1`.)
```
* Local channel that crosses over (related to previous point)
```
def Local_crossover : (-/T1 a, T2/- b)
  channel c : T1/T2
    a! c?
    c! b?    
```
* Self-interpreter **FIXME: work in progress**
```
def Interpret : {m, n} => (bits/- program, [bits/bits]{m} a, [bits/bits]{n} b)
  // 1. read program from bits.
  // 2. based on this program, transduce between a and b.
```
Equivalently, an idea for encoding a Turing machine?
```
def Turing : (bits/bits a)
  channel mem : bits/bits
```
Instead of queue would need 2 stacks to model the tape.
Pass these as channels to the `turing` process.
```
def Turing : (int/int backtape, int/int foretape)
  let initial_value = 1
  channel mem : int/int
    mem ! initial_value
      case mem? of
        | 1 =>
            mem! 2
              foretape! backtape? + 5 // Move head backwards, transforming the tape
        | 2 =>
            mem! 20
              foretape! backtape? * 2 // Move head backwards, transforming the tape
        | n =>
            mem! n - 2
              backtape! foretape? // Move tape forwards
```

# Spec
Important limitations, based on features that are not needed:
* Functions are not values, and we do not represent their types explicitly. Functions appear as parameters to folds, but these functions cannot be named or passed as values (e.g., down a channel).
* Functions are not recursive. Recursive functions _can_ be defined, but only using primitives -- all of which rely on some kind of fold.
* Channels are not values. Channels are either specified by the type of a process, in which case they are connected to some outside source (by the runtime system), or they are local. Channels are not values -- they may not be passed over channels.
* Types cannot be recursive.

## Types
Types form a pointed infinite lattice. The top type is `bits`, consisting of arbitrary bitstrings, and the bottom type is `-`, the empty type. Between them we find the _base types_ consisting of `unit`, `bool`, `int`, `char`, `float` and `string`; and the _structured types_ consisting of lists, labelled finite products and labelled finite sums. Recursive types cannot be defined.

Base and structured types are not generally mutually comparable. One could say that `-` <= `int` <= `bits`, but neither `int` <= `char` nor `char` <= `int`. On the other hand, record comparisons could be allowed, if record subtyping is to be allowed.

The lattice models what kind of casting is allowed. (Thus, for allowable record comparisons we can have a subtyping relation between two record types.) Note that, as usual, _upcasting_ is allowed. This language forbids _downcasting_ and does not support primitives that allow programmers to force this. Thus if `x` is a symbol typed `int` we are allowed to send it over a channel typed `-/bits`, but we cannot ever cast arbitrary bitstrings into a base type, for example. Upcasting can be implicit (by having coercions inserted by the compiler).

### Channel types
The type of channels is special. We cannot form _values_ of channel types -- thus we cannot send channels over channels. Channel types are of two kinds:

1. The first kind of channel type consists of a pair of "normal" types `T1` and `T2`, and written as `T1/T2`, where `T1` is the type of values we can receive over the channel, and `T2` is the type of values we may send over the channel. A channel typed `-/-` is of no use to anybody, since we can neither read nor write to it.

2. The second kind of channel type consists of an array of kind-1 channel types. This is written as `[T1/T2]`, where `T1/T2` is a kind-1 channel type. This grammar forbids us from forming arrays of arrays of channels, or other constructions.

#### Channel subtyping
Like ordinary types, channel types are related by subtyping. In the case of channels, the subtyping relation is _contravariant_. For example, consider a process that accepts a channel typed `-/int`. It is fine to give it a channel typed `int/int`, and it will treat it as being typed `-/int`. There is no harm in this: the process will simply never read any values from that channel, by definition of the channels' type. Thus we can cast `int/int` into `-/int`, but it would be wrong to cast `-/int` into `int/int` since we could then try to read from a channel that, by specification, has nothing to give us. Similarly, we can cast `int/int` into `int/-`.

Let `c` be a channel typed `T1/T2`, and `x` be a value typed `T3`. If `T3` <= `T2` then `c ! x` can succeed. The type of `x` is upcast to that of `c`, not vice versa.

### Type dependencies
The language supports a limited form of dependent types, solely to set the size of arrays (of channels) or lists (of values), thus turning them into _vectors_. All type dependencies _must_ be _nonnegative integers_. Dependent types are used to encode relations between different types of values. For example:
```
def p : {n} => ([int]{n}/- a, -/[int]{n} b)
  ...
```

### Synthesising (de)serialisers from type specifications
We rely on:
* fixed sizes for base types,
* `string` and `list` are the only types that need annotations. These specify limits on their size -- either in terms of acceptors in the form of regexes (which _must_ specify the _terminal anchor_), or in terms of a specific size.
    **(FIXME: For simplicity, can we treat strings as lists of characters, as done in ML?)**
* Other structured types can have their sizes inferred based on the types they carry.

## Expressions
**Lexical convention**: process names should start with uppercase alphabetic. Nullary identifiers starting with lowercase alphabetics are either channels or constants. Non-nullary identifiers starting with lowercase alphabetics are built-ins (e.g., `if..then..else`).

Expressions can consist of the following:
* Constants -- there are no variables, only constants. Constants can have any type.
* Theory-specific expressions:
  * bits: `high`,`low`,`shift`,... 
  * Arithmetic: `+`,`-`,`*`,`/`,`%`,`div`,literals
  * char: `to_int`, `from_int`, `up_case`, `down_case`, '<char>'(literals)
  * list: `::`,`@`,`[]`,`hd`,`tl`
  * Records: `{...}`, `#`(projector)
  * Sums: (constructors), `case..of`
  * string: (treat as lists?), literals
  * bool: `true`, `false`, `if..then..else`, `~`, `||`, `&&`
* If `c` is a channel having type at least `T/-` (where `T` is not `-`), then `c?` is an expression of type `T`
* If `c` is a channel having type at least `-/T` (where `T` is not `-`), and `x` a value of type `T`, then `c!x` is an expression of unit type. (Recall that channels cannot be sent over other channels -- they are not first class values, and neither are functions.)
* Channel symbols by themselves are not expressions.
* Cannot have anonymous functions. Functions aren't first class.
* We can close channels (but not open them). If `c` is a channel, then `close(c)` is an expression of unit type. The evaluation of this expression blocks until the channel is empty, and then can be closed. To close a channel regardless of whether it is empty, use `force_close(c)`.
* **Not sure if we need primitives to tell us how many items there are in a channel.**
* If `c` is a channel, then `is_closed(c)` evaluates immediately to a boolean value indicating whether `c` is closed. Note that a channel cannot be _half-closed_ (i.e., we cannot close the, say, incoming part of the channel) but it can be specified as being simplex (i.e., it can be _half-opened_) by specifying it as `T/-` or `-/T`.
* Local channels can be declared using `channel c : T/T`. The symbol `c` is scoped in the associated block. Note that local channels _must_ have type `T/T` for arbitrary `T`, otherwise they would be rather useless. Note that a local channel behaves as a queue. (Possible syntax sugaring: defining the initial contents of a local channel at the point of declaration.)
* Bounded-capacity channels: it might be useful to statically bound the capacity of channels. As a consequence of this, a 1-capacity local channel can be used to emulate a variable.
* parallel blocks: these consist of vertically juxtaposed expressions.
* sequential blocks: these consist of nested (indented) expressions.
* binding `let x = ...` where `x` is a fresh constant, whose scope extends across the associated block, and whose definition is given by the expression `...`.
* If `P` is a process name, and `P(...)` is a suitable invocation of that process (i.e., all the parameters `...` are of the right type), then `P(...)` is a unit-typed expression whose evaluate completes when the evaluation of the process completes. (For a definition of what it means for a process to complete, see the next section.)
* `fold` over values `v` of arbitrary datatypes, and some base value `B` typed `T`:
```
  fold x, so_far in v, B:
    F
```
where `F` is an expression typed `T`.
* parallel fold (over arbitrary datatypes) -- evaluated opportunistically. (This means that the value over which we fold is deconstructed in some permutation of what we'd use if we folded serially.)
```
  pold x, so_far in v, B:
    F
```
* if `...` is a well-formed block, then
```
  repeat n instance m
    ...
```
is the _m_-ary parallel invocation of `P`, with _n_ instances running at one go. _n_ is a positive integer, and _m_ is a positive integer or `forever`. (Note: we can encode this primitive in terms of `fold` and a generator of a list of _m_ values of some type -- say, the unit type.) Note also that it must be the case that `n < m`.
(Previously I used the `^{1/omega}` notation for this construct, but was a bit cryptic, and also was only applied to a process (not to a block), which required us to write more code (defining a process, then iterating on it externally).)

## _Evaluating_ expressions and _executing_ blocks (informal dynamic semantics)
An interpreter for the language implements the following rules to evaluate a process:
* Execute all blocks in parallel.
* To execute a block, start by evaluating the expression guarding the block.
  Advance the block's execution when a guard is satisfied -- that is, execution moves from left to right.
* A block has been _completed_ when we can no longer move right.
* If a block has been completed, then the containing process has been completed.
* If a process has been completed then this is communicated to its invoking process, since the expression related to the completed process evaluates to the unit value.

## Syntactic sugar
There is potential for plenty, since the core language is quite powerful. I think we can emulate most of the expressions supported by LINQ. Ultimately all of this could be boiled down to code templates, mostly involving folds. Using syntactic sugar would be very useful for representing key idioms.

# Misc
## Related work
The language is inspired by Occam, which implements a version of Hoare's CSP.
But unlike Occam, our channels may consist of pairs of channels -- one in each direction. (Occam channels are unidirectional.) This better models the capabilities of our endpoints (which can consist of bidirectional channels -- implemented as TCP sockets) and reduces clutter (since otherwise we'd need to have 2 separate channel entities in the namespace, instead of one that we can read or write to).
See also dataflow languages, flow-based programming, comonads..

By restricting the arthmetic and list types (to a finite field and vectors respectively) i think that expressions in this language could be synthesisable into hardware circuits. (Unless we treat strings as lists, we'd also need to bound the size of strings.) A small additional circuit (comparable to the runtime system) that carries out scheduling for channel communication would also need to be synthesised, but this would be generic.
cf HardCaml, MetaOCaml

## Type definitions <-> (de)serialisers
see P4, Anil's thesis/Melange, c-types, ...

## Emulating functions
Let x be a value of type A.
Do we have `f1 : A -> B` ~?~ `F2 : (A/- a, -/B b)`
Not really, since `F2` can draw multiple values from `a` and send multiple values to `b`.
  e.g. we could have something like this, where `F2` draws 5 elements from channel `a`, turns them into a list, concatenates the list with its duplicate, and sends the elements of the resulting list down `b`.
```
def F2 : ...
  let a_list =
    fold _, so_far in range(5), []:
      a? :: so_far
  in
    for x in a_list @ a_list:
      b ! x
```
In this case, the function counterpart of process `F2` would be expected to have the type `[A] -> [B]`. Thus processes are not simple general functions -- in this case we have a process that can look like two functions. (And we haven't brought in the complication of sending values back through channels.)

## Processes vs functions vs coroutines
Using `repeat`, blocks/processes can be made to run continuously -- i.e., they would always available for scheduling.
Processes here do not maintain state, unlike coroutines. State could be simulated via local channels -- but there the state is not in the process, but rather encapsulated in the channel; it's the channel's state rather than that of the process.
Unlike coroutines, processes aren't governed by control-flow; they are chiefly governed by data-flow.
Implicitly, processes are functions that transform the world.
Processes are implicitly parallel, and there is no need for them to yield.

## Two process models
There are different ways we could interpret the same program, leading to different (but related) behaviours. In the description above I described only one choice, based on what I considered sensible. Here I contrast two choices.

Consider process P, we could:

1. run it once (and rely on omega-valued operator for repeated runs -- this is done by the `repeat` construct above);

2. run it continuously (and as much as possible) by default.

In both -- and especially in (2) -- we need to specify stopping conditions.
In (1), we need to specify repetition conditions. I picked (1) as the default semantics for this language since it offers the programmer greater control over how processes behave.

The semantics we pick affects how we execute blocks within a process. In both semantics we attempt to run all blocks in a process concurrently.

* In (1), we attempt to complete a single block. Once a block has been fully completed, then the process has done its job. (Note that a block can itself invoke another process an infinite number of times, thus making the block take infinite time unless stopping conditions are met. These can be of two kinds: handled (i.e., guarding on channel close) or not (e.g., resource depletion).)

* In (2) there are no completion criteria by default, so after completing a block, we could start that block afresh.

Another reason I chose (1) as the intended semantics is because it avoids a problem with the semantics of (2). Consider the following process sketch:
```
1: def pipe : (int/- input, -/int output)
2:  input? > 3
3:    input? > 5
4:      output! input?      
```
Under both semantics, the process will execute line 4 after having satisfied the guards in lines 2 and 3. For this to happen, the input channel must have contained a number greater than 3 and, at some strictly later point, a number greater than 5. Both semantics will then read another value from `input` and send it on `output`. The two semantics differ in what happens next. Under semantics (1), the process has completed. Under semantics (2), the process restarts the block; but why are we to start from line 2 and not line 3? Should we allow programmers to specify that they'd like to restart the block from line 3? Semantics (1) avoids this decision point.

We could solve this issue by fixing some convention (say, that we will always restart at the outermost block) or syntax (for instance, by labeling blocks and allowing us to specify which containing block to jump to). Indeed, the presence of the `repeat` construct in the language makes this issue arise even in model (1), but not where `repeat` is not used.

Under both semantics, a process could be executed by inlining all the processes it invokes, and then evaluating all parallel blocks. Different blocks could be scheduled to run on different processors, at a granularity that was not specified by the programmer.

## Two channel models
Consider the (abstract) process
```
def P : (T/- a, -/T b)
  ... b! ... a?
```
and the block
```
1:  P(x, y)
2:  P(y, z1)
3:  P(y, z2)
```
where `x`, `y`, `z1`, `z2` are suitably-typed channels.
Channel `y` is fed to the invocations of `P` occurring on lines 2 and 3.

When a value is sent on channel `y`,

1. Can only one invocation receive that value when it reads from `y`?

2. Or do all invocations listening on `y` receive that value?

Perhaps rather than choosing between these two models, we could choose to support both (if the use-cases require this) by forming different sorts of channels -- behaving as (1) or (2) above -- depending on what the programmer needs.
