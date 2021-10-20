"Link, Process are defined by the bootstrap.

Each process has a priority, which are numbers. Higher numbers go first.

Link subclass: #Process
  instanceVariableNames: 'suspendedContext priority myList threadId'.
  "

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
! forkAt: priority
! newProcess
! newProcessWith: argumentArray
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


Object subclass: #Semaphore instanceVariableNames: 'queue count'.

! Semaphore
! initialize
  super initialize.
  queue := LinkedList new

! signal
  [
    queue isEmpty
      ifTrue: [count := count + 1]
      ifFalse: [queue removeFirst resume]
  ] valueNoContextSwitch

! wait
  | process |
  [
    process := Processor activeProcess.
    count > 0
      ifTrue: [count := count - 1]
      ifFalse: [
        process := Processor activeProcess.
        queue addLast: process.
        process suspend]
  ] valueNoContextSwitch
!!

SystemDictionary at: #Processor put: ProcessorScheduler new.

