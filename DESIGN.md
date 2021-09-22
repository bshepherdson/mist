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

### Memory model

There are several types of objects that need storing. We want this list to be
as short as possible, but not shorter.

The big optimization here is treating `SmallIntegers` as eg. 31-bit numbers
using the top bit as a marker. (Eg. top bit `0` means pointers to other objects,
top bit `1` is a 31-bit 2's complement number.) I haven't actually made that
optimization yet, so we'll get there.

All objects in memory (rather than encoded as special pointers) have a common
64-bit header, which is composed of `tXXXXXXX` and `____gggg`. `t` is the type
code, `XXXXXXX` is a 28-bit count (usually), and `g` is a 16-bit generation
count reserved from the GC.

The only really fundamental types are regular objects, pointer arrays and raw
arrays.

- Objects (`t = 0`) contain two or more pointers to other objects. The number
  is implied by their class, and inferred by the GC from their size.
- Pointer arrays (`t = 1`) are the underlying representation of the `Array`
  class. After the header they contain `k/2 - 1` pointers. (Put the other way,
  a `k`-element array has a header specifying `2k + 2` words.
- Raw arrays (`t = 2`) are the underlying representation for `ByteString`s. They
  do not contain pointers, and so are completely opaque to the GC. After the
  header they have simply `k - 2` words of raw data.
- `t = $f` is reserved for various special types, though none are used yet.
    - `$f` at the top allows small negative integers as specific types!

Note that for non-special `t`, `XXXXXXX` gives the total size of the object in
words, including the 4-word header.

#### Regular objects

Regular objects are laid out like this:

```
$00 header as above
$02 GC word
$04 superobject pointer
$06 class pointer
$08 instance variables
... ...
```

That's only the instance variables of *this* class. The *superobject pointer*
points to another object, an instance of my superclass, holding *its* instance
variables.

When methods are looked up on the context, the context has pointers to both the
fundamental receiver, and the object of the class where this method was found,
which might be a superobject.

**NB:** To greatly reduce memory use at a small cost in complexity, there is a
single shared instance of `Object` (which has no instance variables) used as the
superobject for ~all other objects.

#### Pointer arrays

```
$00 header as above, t = 1
$02 GC word
$04 1st element
$06 2nd element
...
$2k k-1th element
```

#### Raw arrays

They have this form:

```
$00 header as above, t = 2
$02 GC word
$04 1st word
$05 2nd word
$06 3rd word
...
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
list of "escaped" pointers. These are tiny pointer arrays that hold a pointer
to where the offending value was stored, and to the next value in the list.

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

This operation is the one that increments the `g` field in the second word of
all the objects (it's initially 0).
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
    - `$04: push standard class` Shorthand for `$03`, pushes a class from a list
      of common classes in the VM (see below).
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

We define an encoding for literal arrays, as follows:

- First, a length in elements, followed by that many elements.
- Each element begins with a word giving its own length in words. That's the
  whole size of the element, including the length word. This is useful for
  skipping through a literal array to get a particular index.
- Next comes a word giving the type:
    - 1: small integer, a 32-bit big-endian literal number (raw, not indirected
      or tagged)
    - 2: chracters, a 32-bit big-endian literal character value (raw)
    - 3: string
    - 4: symbol
    - 5: literal array
- Then follows the data.

Integers and characters have a fixed size. Literal arrays are recursive: after
the type word comes a word giving the number of elements, then that many
elements follow, etc.

Strings and symbols are encoded the same, but loaded differently. We already
have a length for them, which is 2 words longer than the value. That many raw
words follow.

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


### List of standard classes

This is the list of standard classes that can be pushed with `$04`.

```
$00 Object
$01 ProtoObject
$02 Class
$03 ClassDescription
$04 Behavior
$05 NullObject
$06 Boolean
$07 True
$08 False
$09 Transcript

$10 Magnitude
$11 Number
$12 Integer
$13 SmallInteger

$20 BlockClosure
$21 CompiledMethod
$22 MethodContext

$30 Collection
$31 SequenceableCollection
$32 ArrayedCollection
$33 Array
$34 String
$35 ByteString
$36 Symbol
$37 Text
$38 LinkedList
$39 Interval
$3a OrderedCollection
$3b SortedCollection
$40 Set
$41 Dictionary
$42 IdentityDictionary
$43 Bag

$60 Exception
$61 Error
$62 Warning
$63 NotFound
$64 MessageNotUnderstood
$65 Deprecated
$66 ExceptionSet
$67 CannotReturn
$68 BlockCannotReturn
$69 ContextCannotReturn
$6a AbstractMethod
$6b ShouldNotImplement

$80 TestCase
$81 TestAsserter
$82 TestSuite
$83 TestResult
$84 TestFailure
$85 TestSkipped
```


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


