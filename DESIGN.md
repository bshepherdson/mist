# Internal Architecture

Mist, from "Micro Smalltalk" is intended to be a lightweight Smalltalk engine
that runs in Javascript. The "shipping" portion that reaches the user's browser
is hoped to be as compact and fast as possible.

The system is not self-hosting and not runtime programmable. It does not support
separate compilation. Instead, your Smalltalk files and the runtime library are
compiled as a whole program, tree-shaken, and compiled to compact bytecode for
execution on the user's browser.

## Bytecode Format

The bytecode format is deliberately unspecified for now, as are the bytecodes. I
expect the design to go through several iterations as I work to make the output
smaller and the engine faster.

For now, with an eye on debugging and getting v0.1 to work, I'm using bulky
human-friendly(ish) JSON as the bytecode format.

Once production mode becomes a thing, it will likely become a much more compact
and machine-friendly JSON format, or maybe bytecodes in a proper binary format
with a JSON file holding strings, constants, and so on.

## Smalltalk System

Pharo is the reference implementation for libraries etc.

### Javascript Interop

The target platform is Javascript, and there will be a VM primitive that will
call arbitrary Javascript functions. My goal is to have such a primitive form
all (or most) of the body of a method, which can be called from elsewhere in the
system with a regular Smalltalk message, but which passes the same arguments to
the named Javascript function.

Then the application can include a separate Javascript file containing "native"
code you want to call from Smalltalk, as well as calling web platform libraries.

### Javascript Interop 2: Calling In

Calling Smalltalk functions from Javascript is not likely to happen. I want to
make it possible to pass a block to a Javascript function as a callback, and to
unpack a `Promise` (see below), but since the system is not readily programmed
at runtime (message names may not survive (production) compilation, etc.) that's
a non-feature. If you really need to bind a Smalltalk function to be callable
from Javascript, do it by setting a `window` property to a `Block`.



### Threading and Async

In the author's view, Javascript's asynchrony, lack of threads and inability to
block are a gaping hole, and the source of much of the pain in web development.
Rule 1 of performance on other platforms, for example on Android and iOS, is to
do no more work than strictly necessary on the UI thread, so as not to block the
UI and cause jank.

Javascript doesn't give you that power - whoops.

That problem isn't entirely solvable, of course - sometimes a big network
payload comes in and the browser pauses. But since this system runs in a virtual
machine, it can have threads and operate by time-slicing (using
`requestIdleCallback` where available).

Then all Smalltalk code can operate synchronously and pretend to block, even
though under the hood there are callbacks and such.


### Execution Model

Here's a first hack at the runtime structures to help organize my thoughts.

It's a stack machine with a few local registers (eg. self).

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
- A thread points to its current activation record.
- The runtime is a collection of global values (including classes), and two
  collections of live and dormant threads.


## Scratch List of Bytecodes

Here's the bytecodes I know are needed.

1. Push instance variable.
1. Push argument or temporary.
1. Push a global, such as a named class.
1. Store to an instance variable.
1. Store to a temporary variable. (It's the parser's job to prevent overwriting
   arguments, the VM does it cheerfully.)
1. Send a message (arguments and receiver already on the stack).
1. Send to `super`.
    - This is a special case because `super` is not a real value that can be
      pushed onto the stack.
1. Create a block (needs its arg count and size, plus block arguments and
   temporaries live in the same array as the outer method, to allow enclosing).
1. Duplicate TOS
1. Pop TOS and discard it.
1. Answer TOS.
1. Answer TOS from inside a block.
1. Answer `self`.
1. Pop TOS; if true, skip over a bunch of opcodes and push `nil`.
1. Pop TOS; if false, skip over a bunch of opcodes and push `nil`.
1. Skip forward a fixed number of opcodes.
1. Skip backward a fixed number of opcodes.
1. Execute the given primitive.
1. Pop TOS; if true, skip over a bunch of opcodes and push `true`.
1. Pop TOS; if false, skip over a bunch of opcodes and push `false`.


### Enhancements

It might be useful to add more opcodes to the top level to allow for some
special-case message sends and pushing commonly referenced classes. Little
Smalltalk used this to great effect, with a handful of opcodes and 50 messages
you cover a large fraction of the total message sends in the system with a
compacted form.

On the other hand, memory is not so precious as it once was, so maybe it's fine
to use the vanilla opcodes. I'll have to see the production format in practice
before I can decide on this.


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



