"Exception system design notes:

- Exception is the base class of all signal-able exceptions.
- Error is the base class of all catch-able system failures.
- Abort is the base class of all uncatchable catastrophic events.

Creating Error (subclass) instances DOES NOTHING. An Error instance must be
signaled to begin the process.

Signaling an error captures information about the context where the exception
occurred, and then transfers control to the innermost exception handler.
That context is captured in the instance variables of the Exception.

Exception handlers have several options:
- take some other action and let the original be aborted
- *resume* the interrupted operation (with some edit to the context, presumably)
- *retry* the failed operation
- *pass* the exception to the next-closest handler
- *reraise* a completely different exception *from the original context*!
  - reraising from the handler works too; nothing special about that.

Handler types:
- anyBlock ensure: ensuredBlock
  Always runs ensuredBlock, whether the first block fails or not.
  NB: Result is anyBlock's, not ensuredBlock's.
- anyBlock ifCurtailed: catchingBlock
  Runs if anyBlock returns abnormally, including a non-local return from a block
  These two can be implemented in terms of each other; Deep Into Pharo has it.
- aBlock on: exceptionClass do: handlerBlock
  This is the fundamental exception handling mechanism!
  Returns aBlock's result unless it fails, then handlerBlock's.

Signaling:
- signal is the message that captures the context
"

Object subclass: #Exception.

Exception subclass: #Error instanceVariableNames: 'message'.


! Exception class
! handles: anException
  "By default, an Exception subclass can handle an instance of any transitive
  subclass."
  ^ anException isKindOf: self
! , anotherException
  "Create an exception set to hold all of these."
  | set |
  set := ExceptionSet new.
  set add: self.
  set add: anotherException.
  ^ set
  "^ ExceptionSet new add: self; add: anotherException; yourself"
! messageText: aDescription
  message := aDescription
!!

! Error
! signal
  "TODO Scan for an exception handler on the stack - I'll come back to that."
  self signal: 'An error occurred'
! signal: aMessage
  self messageText: aMessage.
  "TODO"
  nil handleSignal: self
! defaultAction
  Transcript error: self description
! resume
  "TODO"
!!

Object subclass: #ExceptionSet instanceVariableNames: 'exceptions'.
! ExceptionSet
! initialize
  super initialize.
  exceptions := Array new
! , anException
  self add: anException
! add: anException
  exceptions add: anException
! handles: anException
  exceptions do: [:ex | (ex handles: anException) ifTrue: [^ true]].
  ^ false
!!

Error subclass: #MessageNotUnderstood
    instanceVariableNames: 'message receiver'.

! MessageNotUnderstood
! message: aMessage
  message := aMessage
! receiver: anObject
  receiver := anObject
!!

! Object
! doesNotUnderstand: aMessage
  | msg |
  msg := MessageNotUnderstood new.
  msg message: aMessage.
  msg receiver: self.
  msg signal.
  ^ aMessage sentTo: self "In case of a resume, send it again."
!!


"START HERE: Add
Object subclass: #MethodContext instanceVariableNames: 'method pc receiver argv locals parent'
And use it for blocks.

There's a chicken-and-egg problem here, though: the VM must maintain its own
structures for calls and so on, like I have done. It can't create
MethodContexts and call methods on them because that's circular.

Instead the VM does its own, raw activation records, and then when capturing
thisContext it spawns (and caches?) the context for that.
It's used for blocks and exceptions.

No, actually, there's a middle way - create ST objects for it with VM functionality, and then use it as the real context."

