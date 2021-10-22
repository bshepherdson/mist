"Link, Process are defined by the bootstrap.

Each process has a priority, which are numbers. Higher numbers go first.

Link subclass: #Process
  instanceVariableNames: 'suspendedContext priority myList threadId'.
  "

! Process class
! forContext: aContext priority: anInteger
  ^ self new
    suspendedContext: aContext;
    priority: anInteger;
    yourself
!!

! Process
! resume
  <primitive: 54>
! suspend
  <primitive: 55>
! terminate
  | ctx unwindBlock oldList |
  self isActiveProcess
    ifTrue: [
      ctx := thisContext.
      [ ctx := ctx findNextUnwindContextUpTo: nil.
        ctx isNotNil ] whileTrue: [
          (ctx tempAt: 2) ifNil: [
            "NB. Unlike Context>>unwindTo: we do not set complete to true."
            unwindBlock := ctx tempAt: 1.
            thisContext terminateTo: ctx.
            unwindBlock value]].
      thisContext terminateTo: nil.
      self suspend.
      ^ self].

  "Always suspend the process first so it doesn't accidentally get woken up.
  NB. If oldList is a LinkedList then the process is runnable. If it is a
  Semaphore (et al) then the process is blocked, and if it is nil then the
  process is already suspended."
  oldList := self suspend.
  suspendedContext ifNotNil: [
    "If terminating a process halfway through an unwind, try to complete that
    unwind block first."
    (suspendedContext findNextUnwindContextUpTo: nil) ifNotNil: [:outer |
      (suspendedContext findContextSuchThat: [:c | c closure == (outer tempAt: 1)])
        ifNotNil: [:inner |
          "This is an unwind block currently under evaluation."
          self error: 'unimplemented: unwinding blocks in Process>>terminate']].
    ctx := self popTo: suspendedContext bottomContext.
    ctx == suspendedContext bottomContext ifFalse: [
      self error: 'Unwind error during termination'].
    "Set the context to its ending PC for the benefit of isTerminated."
    ctx pc: ctx endPC]

! run
  "Suspend the current process and execute self instead."
  | proc |
  proc := Processor activeProcess.
  [
    proc suspend.
    self resume
  ] forkAt: Processor highestPriority

! priority
  ^ priority
! priority: aPriorityNumber
  priority := aPriorityNumber
  "TODO: Might need to move this process between queues."

! suspendedContext
  ^ suspendedContext
! suspendedContext: aContext
  suspendedContext := aContext

! suspendingList
  ^ myList
! suspendingList: aLinkedList
  myList := aLinkedList

! isActiveProcess
  ^ self == Processor activeProcess

! signalException: anException
  "TODO: This isn't necessary yet and it's a bit tricky."

! isBlocked
  "A process is blocked if it is waiting on a list (eg. a Semaphore) that is not
  the runnable process list."
  | myPriority |
  myPriority := priority. "Capture the current priority, in case of preempting."
  ^ myList
    ifNil: [false]
    ifNotNil: [:list | list ~~ (Processor waitingProcessesAt: myPriority)]

! isRunnable
  | myPriority |
  myPriority := priority.
  ^ myList
    ifNil: [^self == Processor activeProcess]
    ifNotNil: [:list | list == (Processor waitingProcessesAt: myPriority)]
!!


! BlockClosure
! fork
  self newProcess resume

! forkAt: priority
  (self newProcessAt: priority) resume

! newProcess
  ^ self newProcessAt: Processor activePriority

! newProcessAt: priority
  ^ Process
    forContext: [
      self value.
      "Since control is now at the bottom, there is no need to terminate (which
      runs unwinds) since all unwinds have been run. Simply suspend.
      Note that we must use this form rather than eg. Processor suspendActive
      so that isTerminated answers true. isTerminated requires that if there is
      a suspended context it is the bottom-most (ie. no sender), so indirecting
      through suspendActive would make the suspendedContext not bottom-most!"
      Processor activeProcess suspend] asContext
    priority: priority

! newProcessWith: argumentArray
  ^ Process
    forContext: [
      self valueWithArguments: argumentArray.
      "Since control is now at the bottom, there is no need to terminate (which
      runs unwinds) since all unwinds have been run. Simply suspend.
      Note that we must use this form rather than eg. Processor suspendActive
      so that isTerminated answers true. isTerminated requires that if there is
      a suspended context it is the bottom-most (ie. no sender), so indirecting
      through suspendActive would make the suspendedContext not bottom-most!"
      Processor activeProcess suspend] asContext
    priority: Processor activePriority

! asContext
  ^ self asContextWithSender: nil

! asContextWithSender: aContext
  ^ MethodContext new
    method: self;
    locals: ctx locals;
    sender: aContext;
    pc: pc0;
    yourself
!!



"Process scheduling:
- ProcessorScheduler has an Array of LinkedLists of Processes waiting to run.
- Suspended processes are assumed to be in a queue or something. 
"


"Object subclass: #ProcessorScheduler
  instanceVariableNames: 'quiescentProcesses activeProcess'."

! ProcessorScheduler
! initialize
  quiescentProcesses := Array new: 7.
  1 to: 7 do: [:ix | quiescentProcesses at: ix put: LinkedList new]

! activePriority
  ^ self activeProcess priority
! activeProcess
  ^ activeProcess

! nextReadyProcess
  quiescentProcesses reverseDo: [ :list |
    list isEmpty ifFalse: [ | proc |
      proc := list first.
      proc suspendedContext ifNotNil: [ ^ proc ]]].
  ^ nil

! waitingProcessesAt: aPriority
  ^ quiescentProcesses at: aPriority

! terminateActive
  "TODO anything special?"
  self activeProcess terminate

! yield
  | semaphore |
  semaphore := Semaphore new.
  [semaphore signal] fork.
  semaphore wait

"Priority names"
! highestPriority
  ^ self timingPriority
! timingPriority
  ^ 7
! highIOPriority
  ^ 6
! lowIOPriority
  ^ 5
! userInterruptPriority
  ^ 4
! userSchedulingPriority
  ^ 3
! userBackgroundPriority
  ^ 2
! systemBackgroundPriority
  ^ 1
!!


"LinkedList subclass: #Semaphore instanceVariableNames: 'excessSignals'."

! Semaphore
! initialize
  super initialize.
  excessSignals := 0

! signal
  <primitive: 56>
! wait
  <primitive: 57>

! critical: mutexedBlock
  "Evaluate mutexedBlock only if the receiver is not currently in the process of
  running the critical: message. If the receiver is, evaluate mutexedBlock after
  the other critical: message is finished."
  self wait.
  ^ mutexedBlock ensure: [self signal]

! critical: mutexedBlock ifLocked: lockedBlock
  0 = excessSignals ifTrue: [ ^ lockedBlock value ].
  excessSignals := excessSignals - 1.
  ^ mutexedBlock ensure: [ self signal ]
!!

SystemDictionary at: #Processor put: ProcessorScheduler new.


Object subclass: #SharedQueue
  instanceVariableNames: 'contents readPos writePos mutex empty'.

! SharedQueue class
! new
  ^ self new: 10
! new: size
  ^ self basicNew initialize init: size; yourself
!!

! SharedQueue
! init: size
  contents := Array new: size.
  writePos := readPos := 1.
  mutex := Semaphore new.
  empty := Semaphore new.
  mutex signal

! isEmpty
  ^ readPos = writePos

! next
  | value |
  empty wait.
  ^ mutex critical: [
    readPos = writePos
      ifTrue: [
        self error: 'SharedQueue synchronization error - queue is empty!'.
        value := nil]
      ifFalse: [
        value := contents at: readPos.
        readPos := readPos + 1].
    value ]

! nextPut: value
  "Send a value through the receiver. If a Process has been blocked waiting to
  receive a value through the receiver, allow it to proceed."
  mutex critical: [
    writePos > contents size ifTrue: [self makeRoomAtEnd].
    contents at: writePos put: value.
    writePos := writePos + 1].
  empty signal.
  ^ value

! peek
  "Answer the object that was sent through the receiver first and has not yet
  been received by anyone; but don't remove it from the receiver. Return nil if
  there's nothing there."
  ^ empty
    critical: [
      mutex critical: [
        readPos >= writePos ifFalse: [
          contents at: readPos ] ] ]
    ifLocked: [ nil ]

! makeRoomAtEnd
  | contentsSize newArray |
  contentsSize := writePos - readPos.
  newArray := contentsSize * 2 > contents size
    ifTrue: [contents class new: contents size * 2]
    ifFalse: [
      (contents size > 10 and: [contents * 4 <= contents size])
        ifTrue: [contents class new: (contentsSize * 2 max: 10)]
        ifFalse: [contents]].
  newArray
    replaceFrom: 1
    to: contentsSize
    with: contents
    startingAt: readPos.
  contents == newArray
    ifFalse: [contents := newArray]
    ifTrue: [contents from: contentsSize + 1 to: contents size put: nil].
  readPos := 1.
  writePos := contentsSize + 1
!!

