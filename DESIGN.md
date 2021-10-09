# Internal Architecture

Mist, from "Micro Smalltalk", is intended to be a lightweight Smalltalk engine
that runs on 32-bit or larger microprocessors.

## Smalltalk System

Pharo is the reference implementation for libraries etc. Pharo is not a standard
Smalltalk-80 system! But it is industry-leading and a solid point of reference.

Aside from some minor library evolutions, the big difference between the
standard and Pharo is that **superclass instance variables are not accessible**.

### Self-hosting development environment

The goal is, eventually, to get the entire system to be self-hosting,
bootstrapped from a host machine to build an *image* that can be reloaded.

For now, though, code lives in host files and is streamed at startup.

### Current runtime

Right now the target environment is actually Javascript! This is because it
comes with a powerful debugger and supports rapid, cross-platform development.

The microprocessor's memory is simulated with a big Uint16Array, and all host
interop lives in primitives, insulating most of the Smalltalk code from the
host machine.

Note that Uint16Array uses the **host machine's** byte order. For ~all computers
in use today, that's little-endian. That's not the binary order for eg. DCPU-16,
however.

### Compiler

Currently there's no compiler written in Smalltalk - instead there's a compiler
written in Go that consumes Smalltalk files and emits the bytecode as a binary
stream suitable for sending to an arraybuffer.

The compiler writes the 16-bit bytecodes in little-endian order, to play nice
with `Uint16Array` in Javascript.

## Execution Model

The virtual machine is a stack machine, with a few built-in registers (mostly
`thisContext`).

Building from the bottom up:

- A bytecode is a single, simple operation. It has 0 or more operands.
    - Some examples: pushing a local's value to the stack, storing top-of-stack
      in a local, pushing `self` or `super`, sending a message (given the
      argument count), answer a value, answer self.
- A method or block is a stream of bytecodes, plus 0 or more locals.
    - Blocks' code is embedded into the stream of their parent methods.
- A class has a package, name, superclass, and two collections of methods (class
  and instance level).
- An instance has a pointer to its class, and a collection of instance
  variables.
- An "activation record" (also known as a "stack frame" in other environments)
  represents a single execution of a given method. It has a pointer to the
  current bytecode within that method, an array of values for `self`, the
  message parameters, any local variables, and a stack of working values. It
  also points to its parent activation record, which is answered to when values
  are needed.
    - The stack frame is a Smalltalk object!
- A thread points to its current activation record.
- The runtime is a collection of global values (including classes), and two
  collections of live and dormant threads.

## Memory model

There are several types of objects that need storing. We want this list to be
as short as possible, but not shorter.

The big optimization here is treating `SmallIntegers` as eg. 31-bit numbers
using a bit as a marker. (Eg. top bit `0` means pointers to other objects,
top bit `1` is a 31-bit 2's complement number.) The top bit is used because an
important target CPU (my invented Mocha 86k 32-bit DCPU successor) has a
compact/fast `BNG` "branch negative" instruction that can be used to test this.

### Object Formats

All objects in memory (rather than encoded as special pointers) have a common
64-bit header.

```
ssssssss __hhhhhh hhhhhhhh hhhhhhhh
ggggffff __cccccc cccccccc cccccccc
```

- `ssssssss` is an 8-bit slot count. 255 is taken to mean "another 32-bit long
  giving the count precedes the header".
  - The extra size is at `[p-2]`, before the header. 32-bit unsigned size.
  - In order to make a block of contiguous objects crawlable, an extra longword
    lives at `[p-4]`, with `$ff000000` in it. Thus if the first long of a
    would-be object has `$ff______`, it's a long form header. The real object is
    4 words farther along.
- `hhhhh...` is a 22-bit "identity hash". This is a magic number assigned to
  every object at allocation time.
  - For classes specifically, this is equal to the class index (see below).
  - For all other objects, it's an arbitrary number intended for use in hashing
    collections, assigned in some arbitrary way by the allocator.
- `gggg` is reserved for the GC to store information and tags.
- `ffff` is the object format, giving the flavour of the object:
    - `0`: 0-sized object (`nil`, `true`, `false`, etc.)
    - `1`: fixed size, with instance variables (eg. `Point`, a classic object)
    - `2`: variable sized, no inst vars. (eg. `Array`)
    - `3`: variable sized with inst vars too (eg. `MethodContext`)
    - `4`: unused for now. (Spur: *weak* variably-sized, eg. `WeakArray`)
    - `5`: unused for now. (Spur: *weak* fixed-sized with inst vars, eg. `Ephemeron`)
    - `6-7`: 16-bit indexables.
        - There are two here, because the size field (`ssssssss` or the next
          long) is in *longs* for all objects, and this notes whether this size
          is a rounding error or not. This is effectively subtracted from the
          length in 16-bit words: `ssssssss << 1 - (ffff & 1)`.
    - `8-15` compiled methods with various forms.
        - This property is cribbed from Spur without much analysis - not sure
          yet what they're used for there. Possibly JIT things I don't need.
- `ccccc...` is a 22-bit class index. All classes are assigned a number
  permanently in this space, and it never changes.
    - As noted above, the class has its identity hash (`hhhh...`) set to its own
      class index. That means that `SomeClass basicNew`, which gets a pointer to
      the class, can trivially find the class index as well (`self
      identityHash`) *and* as a bonus there will never be an identityHash
      collision for classes!

#### 0-size objects (0)

These are simply the 64-bit header. They have no instance variables, and the
class is everything one needs to know. `true`, `false`, `nil`, and others.

This isn't special, and the VM will allocate this form if a user class has no
(transitive) instance variables. (Eg. Lexer tokens and other markers with no
data attached.)

#### Fixed-size with instance variables (1)

This is a very common type; most objects are this.

They are laid out like this:

```
$00 header as above
$02 1st instance variable
$04 2nd instance variable
...
```

The indexes of instance variables start at 1, like Smalltalk arrays, so they are
effectively (long) offsets into the object.

#### Variable size, no instance variables

This is `Array` and similar classes,

```
$00 header as above
$02 1st element
$04 2nd element
...
$2k k-1th element
```

#### Variable size with instance variables

This is unique(?) to `MethodContext` currently, which has a handful of instance
variables and then a stack array inlined.

The `ssssssss` size field in the header gives the number of instance variables;
the extra size long gives the number of variable fields.

```
$-4   `$ff000000` marker
$-2   variable count (`v`)
$00   header (`s` size)
$02   1st inst var
$04   2nd inst var
$06   3rd inst var ...
...
2s+2  1st variable field
...
2s+2v last variable field
```

## Memory management and GC

Intention is to use five-space generational GC. Given a total memory of `M`,
define `M = 16m`. Then with ascending addresses:

- Special region: `1m` for microcomputer code, special objects outside the GC's
  scope, etc.
- Tenured region: `3m` for Smalltalk objects that have achieved "tenure" either
  because they're built-in or because they've survived a long time.
    - Can grow slowly in theory, but is probably fixed in practice.
- Old generation: `2m` of objects previously copied, the old disused copy.
- New generation: `2m` of objects previously copied, copied more recently and
  currently in use.
- Eden: `8m` for fresh allocations

It's important that the Eden comes after all other memory! Then simple
comparison can be used to determine if a pointer is to an old object or to the
Eden.

### Allocation

From the high end of Eden to the low end. If we're about to allocate something,
but the adjusted pointer is `< EDEN_LIMIT` then trigger a GC.

This keeps allocation very cheap - no free lists or screwing around. Just adjust
and check the pointer. Ideally both the allocation and Eden limit can live in
machine registers for speed.

### Old to new

Bytecodes that cause pointer writes (eg. storing instance variables, primitives
writing to pointer arrays) need to check if we're storing a pointer from the
survivor or tenure spaces into the Eden.

In that case, they allocate a new special value, essentially creating a linked
list of "escaped" pointers. These are `Association`s, 2-element arrays that hold
a pointer to where the offending value was stored, and to the next value in the
list.

Then during a collection these lists can be used to find and update all
references to new objects in old spaces. This is a vital GC root.

### GC roots

The only GC roots are the VM threads' contexts, and the above "escaped pointer"
list.

### Minor GC

The tail of the "new generation" is the target. Scan the GC roots (see above)
and copy all reachable objects in the Eden into the new generation. (It's
assumed to fit! Crash if it doesn't.)

It's expected that nearly everything will be dead, and very few objects will
actually survive. Most of the allocations are stack frames and other throwaway
temporaries.

Redirection is achieved by replacing the first long of an object in the Eden
with 0, and the second long with a pointer to its new location.

### Major GC

When a GC is required, and the new generation is over some fullness threshold,
we perform a major GC instead. This copies *both* the new generation and Eden
onto the old generation. Then the roles of new and old generation are swapped.

This is a more expensive operation, since it's expected that most of the
survivors continue to survive.

This operation is the one that increments the `gggg` field in the header of all
the objects (it's initially 0).
That field is a counter used to decide on tenuring.

### Tenuring

When an object has been copied `MAX_COPIES` times during a major collection, it
is copied to the tenured space instead, and not copied again. This is to
preserve long-lived objects without wasting cycles copying them repeatedly.

#### Tenure GC

It's possible, especially for a long-lived session/image that the tenured space
needs a GC as well. This is probably easiest to achieve with a mark/sweep
compacting copy, implemented separately. The Eden can be used as a temporary
area, essentially copying all reachable tenured objects to Eden, then copying
them back. That's *expensive* but it should be extremely rare.

For now, that's all unimplemented, and if the tenuring space fills up the system
will simply crash.


## Bytecode, or actually Wordcode

I'm going to keep using the term *bytecode*, but the machines really being
targeted here actually use 16-bit words.

We divide that word into 3 fields as follows: 4-bit *opcode*, 4-bit *count*,
8-bit *operand*. (Those names are used, even though they don't always apply.)

Several opcodes actually treat the count as a sub-opcode. Not all opcodes use
the operand field at all.


- `$0: push` A whole family of opcodes that push values onto the interpreter
  stack.
    - `$00: push special` Push special values
        - `$0000: push self`
        - `$0001: push thisContext`
        - `$0002: push nil`
        - `$0003: push true`
        - `$0004: push false`
        - Lots of room for expansion here.
    - `$01: push local` Pushes local whose number is in the operand.
    - `$02: push instance variable` Pushes instance variable whose number is in
      the operand.
        - Note that only this specific class's instance variables are
          accessible! Superclasses cannot be accessed directly.
    - `$03: push global` Push a global value looked up in the `SystemDictionary`
      by the symbol in the literal table at *operand*.
      table in the VM - see below for that list.
    - `$04: push standard class` Shorthand for `$03`, pushes a class by its
      literal class index. This has room for 256 built-in classes.
    - `$05: push literal` Push the literal from this method's literals list
      whose index is in *operand*.
    - `$06: push inline number` Interpret *operand* as a signed 8-bit value and
      push it. That gives a range of -128 to 127, which is lots.
- `$1: store` Store to locals and instance variables
    - `$10: store local` Store TOS into local variable whose index is in operand
      field. NB: It's illegal to store over arguments or `self`.
    - `$11: store instance variable` Store TOS into instance variable whose
      index is in operand field.
- `$2: send` Fundamental message sending instruction.
    - Count is the argument count (not including `self`)
    - Operand is the index of the message selector (a `Symbol`) in the literals.
- `$3: super send` Identical to `send`, but sends to `super` instead.
- `$4: canned send` Like `send`, count is the argument count. But the operand
  is an index into a table of up to 256 common messages in the VM. (There are
  separate lists for arg counts up to 2; see below.)
- `$5: start block` Begins a block, whose bytecodes are inlined after this one.
  The *count* gives the argument count of the block; *operand* its argument
  start index. The **next** word gives the count of bytecodes following.
    - There's no end tracking; the block is assumed to contain code to return
      properly.
- `$6: misc ops` A collection of fundamental operations. *operand* is
  ignored for these.
    - `$60: dup` Duplicate TOS, pushing a copy of it.
    - `$61: drop` Drop TOS from the stack.
    - `$62: answer TOS` Answer TOS from the current context (block or method).
    - `$63: Answer self` Shorthand for `push self` then `answer TOS`.
    - `$64: Block answer TOS` Block return, returning from the whole method.
    - **TODO**: "answer instance variable N" is probably a good shorthand, it
      reduces lots of trivial methods to a single opcode.
      - Double-**TODO**: That could be even more compact by encoding the methods
        as just a header or something, so when "calling" them, no stack
        machinery is needed.

- `$6: branches` More operations under `$6`. These use *operand* as a skip
  count.
    - `$68: Skip true push nil` Pop TOS; if true, skip over a bunch of opcodes
      and push `nil`.
    - `$69: Skip false push nil` Pop TOS; if false, skip over a bunch of opcodes
      and push `nil`.
    - `$6a: Skip forward` Skip forward *operand* opcodes.
    - `$6b: Skip backward` Skip backward *operand* opcodes.
    - `$6c: Skip true push true` Pop TOS; if true, skip over a bunch of opcodes
      and push `true`.
    - `$6d: Skip false push false` Pop TOS; if false, skip over a bunch of
      opcodes and push `false`.

- `$7: primitives` Invokes primitive operations in the VM. *operand* gives the
  primitve number, *count* currently unused.

Lots of unused space, if I discover useful things.

### Encoding of literals

We need a scheme for loading literal values produced outside Smalltalk into the
system.

Each element begins with a word giving its type:

- 1: small integer, a 32-bit big-endian literal number (raw, not indirected
  or tagged)
- 2: chracters, a 16-bit big-endian literal character value (raw)
- 3: string
- 4: symbol
- 5: literal array
- 6: true
- 7: false
- 8: nil
- 9: word array

True, false and nil have no data at all. Integers are 2 words, characters are 1.
Literal arrays are recursive: after the type word comes a word giving the number
of elements, then that many elements follow, etc.

Word arrays, strings and symbols are encoded the same, but loaded differently.
The first word after the type gives the length in words, and that many raw
words follow. (ASCII values in strings and symbols, arbitrary in word arrays).

The system interprets these and copies them into memory.


### Driver input

The external, bootstrapping compiler wants to produce a binary file, which we
then stream into the Smalltalk engine a word at a time. It can't contain any
(non-relative) pointers, since it needs to be relocatable.

The driver is a very simple switch on numerical values encoded in the literal
format above.

A literal array of the form

```
#( 1
   #SomeClass
   #( #this:methods:selector argc locals #( 12 3289 91 ) #( #size #= #42768 )
   #( --more like the above-- )
   )
```

defines methods on a class. `1` is the command, the next value is the symbol for
the class, and each value after that is a method: selector, arg count, local
count, bytecode and literals. These are copied and interpreted into the VM.

`2` is the command for adding class-level methods to `SomeClass class`. (The
symbol still reads `#SomeClass`.)

`3` is the command to execute some inline code with no name. This is used for
user commands as well as for defining subclasses.

```
#( 3 #( bytecode ) #( literals ) )
```

I don't think any more commands are necessary for now.


### Enhancements

It might be useful to add more opcodes to the top level to allow for more
special-case message sends and pushing commonly referenced classes. Little
Smalltalk used this to great effect, with a handful of opcodes and 50 messages
you cover a large fraction of the total message sends in the system with a
compacted form.

On the other hand, memory is not so precious as it once was, so maybe it's fine
to use the vanilla opcodes. I'll have to see the production format in practice
before I can decide on this.


## Known gaps

- Block returns do not `ensure:` or `ifCurtailed:` correctly!
    - Requires sending messages from the bytecode implementation as it gets
      unwound. Perhaps a quick-and-dirty check for any handlers in scope, and
      using `thisContext resume: returnValue through: targetContext`.

    - In the common case where there's no handlers, this can be implemented in
      the VM for speed.



## Appendix

This is older material from earlier stages of the design.

### Compiler Notes

Processing a message send is simple enough:

- Unary: receiver is pushed, so send it.
- Binary: expect the argument, push it and send.
- Keyword: receiver is pushed, then each argument in turn. When we reach the end
  of the phrase, combine the keywords to build the message name, and send.

Cascades are a little tricky; they come out of the parser as a stream of message
sends without a receiver. We want to add a `dup` bytecode between the receiver
and each send but the last.

Here's the plan:

- Have a stack of flags representing the current nesting of cascades and
  subexpressions.
- On entering a cascade, push true.
- On entering a subexpression, push false.
- On leaving either, pop.
- If the flag is set, `dup` before each `unaryTail`, `binaryTail` or
  `keywordMessage` and `drop` after it, except the last one.

We need a similar stack for keyword message names, since they can be nested as
subexpressions.


