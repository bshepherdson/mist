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

! BlockClosure
! on: exception do: handlerAction
  "Evaluate the receiver in the scope of an exception handler.
  This temporary value is necessary! It's used by way of tempAt: 3 in the
  MethodContext>>#exceptionHandlerIsActive methods."
  | handlerActive |
  handlerActive := true.
  ^ self value

! ensure: aBlock
  "Evaluate a termination block after evaluating the receiver, regardless of
  whether the receiver's evaluation completes."
  | complete returnValue |
  returnValue := self valueNoContextSwitch. "TODO: Add atomic flag to threads"
  complete ifNil: [
    complete := true.
    aBlock value ].
  ^ returnValue

! ifCurtailed: aBlock
  "Evaluate the receiver with a guard in case of abnormal termination.
  aBlock is evaluated only if execution is unwound during execution of the
  receiver. If execution of the receiver finishes normally aBlock is not
  evaluated."
  | complete result |
  result := self valueNoContextSwitch. "TODO: Atomic flag"
  complete := true.
  ^ result

! isUnwinder
  ^ false

! isExceptionHandler
  ^ false
!!

! CompiledMethod
! isExceptionHandler
  ^ self selector = #on:do:
! isUnwinder
  ^ self selector = #ensure: or: [self selector = #ifCurtailed:]
!!

Object subclass: #Exception
    instanceVariableNames: 'messageText signaler signalContext handlerContext'.

Exception subclass: #Error.
Exception subclass: #Warning.

Warning subclass: #Deprecated.


! Exception class
! handles: anException
  "By default, an Exception subclass can handle an instance of any transitive
  subclass."
  ^ anException isKindOf: self
! signal
  ^ self new signal
! signal: aMessage
  ^ self new signal: aMessage
! , anotherException
  "Create an exception set to hold all of these."
  | set |
  set := ExceptionSet new.
  set add: self.
  set add: anotherException.
  ^ set
  "^ ExceptionSet new add: self; add: anotherException; yourself"
!!

! Error
! isResumable
  "By default, errors cannot be resumed. Some types of errors override this, if
  they can be repaired."
  ^ false
!!

"Pharo has ContextPart as a superclass; I don't know if that split is useful."
! MethodContext
! initialize
  sp := 0

! endPC
  ^ method bytecode size
! pc: aNumber
  pc := aNumber

! method
  ^ method
! method: aCompiledMethod
  method := aCompiledMethod

! locals
  ^ locals
! locals: anArray
  locals := anArray

! bottomContext
  ^ self findContextSuchThat: [:c | c sender isNil]

! nextHandlerContext
  "Finds the innermost exception handling context. That is, one with on:do:."
  ^ self sender findNextHandlerContextStarting

! findContextSuchThat: aBlock
  | ctx |
  ctx := self.
  [ ctx isNil ] whileFalse: [
    (aBlock value: ctx) ifTrue: [^ctx].
    ctx := ctx sender].
  ^ nil

! compiledCode
  ^ method
! sender
  "Returns the parent context, that sent the message that created this one."
  ^ sender

! sender: aContext
  sender := aContext

! selector
  ^ self compiledCode selector

! hasSender: context
  | senderContext |
  self == context ifTrue: [^false].
  senderContext := sender.
  [senderContext == nil] whileFalse: [
    senderContext == context ifTrue: [^true].
    senderContext := senderContext sender].
  ^ false

! tempAt: index
  "Self is the first local, so we need to increase the index by 1."
  ^ locals at: index + 1
! tempAt: index put: aValue
  "Self is the first local, so we need to increase the index by 1."
  ^ locals at: index + 1 put: aValue

! receiver
  "Receiver is the first local."
  ^ locals at: 1

! findNextHandlerContextStarting
  "Return the next handler marked context, returning nil if there is none.
  Search starts with self and proceeds up to nil."
  | ctx |
  ctx := self.
  [ ctx isHandlerContext ifTrue: [^ctx].
    (ctx := ctx sender) isNil ] whileFalse.
  ^ nil

! exceptionClass
  ^ self tempAt: 1
! exceptionHandlerBlock
  ^ self tempAt: 2

! exceptionHandlerIsActive
  "This is actually the first local of BlockClosure>>#on:do: which falls at
  index 3 in the args/locals list, which is [self, exception, handler, active]."
  ^ self tempAt: 3

! exceptionHandlerIsActive: aBoolean
  ^ self tempAt: 3 put: aBoolean

! unwindComplete
  "This is actually the first local of BlockClosure>>#ensure: and
  BlockClosure>>#ifCurtailed:, index 2 in the list:
  [self, ensureBlock, complete]."
  ^ self tempAt: 2

! unwindComplete: aBoolean
  ^ self tempAt: 2 put: aBoolean

! isHandlerContext
  ^ method isExceptionHandler

! isUnwindContext
  ^ method isUnwinder

! isDead
  "Returns true if this context is 'dead', that is if it cannot be resumed or
  block-returned. This can happen if a block escapes its creator."
  ^ pc isNil

! handleSignal: exception
  "Sent to handler (on:do:) contexts only. If my exception class (first arg)
  handles exception then execute my handle block (second arg), otherwise forward
  this message to the next handler context. If none are left, execute the
  exception's defaultAction (see nil>>handleSignal:)."
  | value |
  ((self exceptionClass handles: exception)
      and: [self exceptionHandlerIsActive])
    ifFalse: [ ^ self nextHandlerContext handleSignal: exception ].

  exception prevHandlerContext: self.
  value := self exceptionHandlerBlock cull: exception.
  "return from self if not otherwise directed in handle block"
  self return: value

! cannotReturn: value to: aContext
  ^ ContextCannotReturn result: value to: aContext

! return: value
  "Unwind thisContext to self and return value to self's sender. Execute any
  unwinding blocks while unwinding (eg. ensure:). ASSUMES self is a sender of
  thisContext!"
  sender ifNil: [self cannotReturn: value to: sender].
  sender resume: value

! resume: value
  "Unwind thisContext to self and resume with value as the result of the last
  send. Execute unwind blocks when unwinding. ASSUMES self is a sender of
  thisContext."
  self resume: value through: (thisContext findNextUnwindContextUpTo: self)

! resume: value through: firstUnwindContext
  "Unwind thisContext to self and resume with value as result of last send.
  Execute any unwind blocks while unwinding.
  ASSUMES self is a sender of thisContext."
  | context unwindBlock |
  self isDead ifTrue: [self cannotReturn: value to: self].
  context := firstUnwindContext.
  [ context isNil ] whileFalse: [
    context unwindComplete ifNil: [
      context unwindComplete: true.
      unwindBlock := context unwindBlock.
      thisContext terminateTo: context.
      unwindBlock value].
    context := context findNextUnwindContextUpTo: self].
  thisContext terminateTo: self.
  ^ value

! unwindBlock
  "Grabs the argument to ensure: and ifCurtailed:, index 1, the first arg."
  ^ self tempAt: 1

! findNextUnwindContextUpTo: aContext
  | ctx |
  ctx := self.
  [ (ctx := ctx sender) isNil or: [ctx == aContext] ]
    whileFalse: [ ctx isUnwindContext ifTrue: [^ctx] ].
  ^nil

! terminateTo: previousContext
  "Terminate all the Contexts between me and previousContext, if previousContext
  is on my Context stack. Make previousContext my sender."
  | currentContext sendingContext |
  (self hasSender: previousContext) ifTrue: [
    currentContext := sender.
    [currentContext == previousContext] whileFalse: [
      sendingContext := currentContext sender.
      currentContext terminate.
      currentContext := sendingContext]].
  sender := previousContext

! terminate
  "Make myself unresumable."
  sender := nil.
  pc := nil
!!

! Exception
! messageText: aDescription
  messageText := aDescription
! receiver
  ^ self signalerContext receiver
! signal
  signalContext := thisContext.
  signaler ifNil: [signaler := self receiver].
  ^ thisContext nextHandlerContext handleSignal: self
! signal: aMessage
  self messageText: aMessage.
  self signal

! signalerContext
  "Find the first sender of signal(:), the first context which is neither for
  an instance method nor class method of Exception (and subclasses).
  This will make sure the same context is found for both `Error signal` and
  `Error new signal`."
  ^ signalContext findContextSuchThat: [:ctx |
    (ctx receiver == self or: [ctx receiver == self class]) not]

! defaultAction
  self defaultActionInner
! defaultActionInner
  <primitive: 21>

! prevHandlerContext
  ^ handlerContext
! prevHandlerContext: aContext
  handlerContext := aContext
! resume
  "TODO"
  Transcript show: 'resume is not implemented'.
  self halt
! return: aValue
  handlerContext return: aValue
! pass
  "Yields control to the enclosing exception action for the receiver."
  | nextHandler |
  nextHandler := handlerContext nextHandlerContext.
  handlerContext := nil.
  nextHandler handleSignal: self

! freeze
  "Does nothing right now. TODO in the case I rebuild stacks."
!!


Object subclass: #ExceptionSet instanceVariableNames: 'exceptions'.
! ExceptionSet
! initialize
  super initialize.
  exceptions := OrderedCollection new
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

Error subclass: #CannotReturn
    instanceVariableNames: 'result'.
CannotReturn subclass: #BlockCannotReturn
    instanceVariableNames: 'home'.
CannotReturn subclass: #ContextCannotReturn
    instanceVariableNames: 'target'.

! CannotReturn
! result
  ^ result
! result: aValue
  result := aValue
! isResumable
  ^ true
!!

! BlockCannotReturn
! home
  ^ home
! home: aValue
  home := aValue
!!

! BlockCannotReturn class
! result: aValue from: homeContext
  ^ ((self new result: aValue) home: homeContext) signal
!!

! ContextCannotReturn
! target
  ^ target
! target: aValue
  target := aValue
!!

! ContextCannotReturn class
! result: aValue to: aContext
  ^ ((self new result: aValue) target: aContext) signal
!!

! UndefinedObject
! handleSignal: anError
  anError resumeUnchecked: anError defaultAction
!!



Error subclass: #SelectorError instanceVariableNames: 'selector'.

! SelectorError class
! signalFor: aSelector
  | ex |
  ex := self new.
  ex selector: aSelector.
  ^ ex signal
!!

! SelectorError
! selector
  ^ selector
! selector: aSelector
  selector := aSelector
! messageText
  ^ messageText ifNil: [messageText := self standardMessageText]
! standardMessageText
  ^ 'Selector error for ' , selector
!!

SelectorError subclass: #SubclassResponsibility.
! SubclassResponsibility
! standardMessageText
  ^ signaler class name , ' >> ' , selector , ' is abstract'
!!

SelectorError subclass: #ShouldNotImplement.


! ShouldNotImplement
! standardMessageText
  ^ signaler class name, ' should not implement ', selector
!!


! Object
! subclassResponsibility
  ^ SubclassResponsibility signalFor: thisContext sender selector
! shouldNotImplement
  ^ ShouldNotImplement signalFor: thisContext sender selector
! error: anException
  anException signal
!!

! String
! signal
  ^ Error signal: self
!!

