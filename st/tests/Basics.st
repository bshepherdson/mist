TestCase subclass: #Basics1.

! Basics1
! testPrint
  Transcript show: 7
! testSums
  Transcript show: 7
! testArgs
  Transcript show: 7
!!

"Transcript show: Basics1 run."

Object subclass: #Bexp.

! Bexp
! mainBlock
  ^ [Transcript show: 'mainBlock start'.
    self failingBlock ensure: self ensureBlock.
    Transcript show: 'mainBlock end']

! failingBlock
  ^ [Transcript show: 'failingBlock start'.
    Error signal.
    Transcript show: 'failingBlock end']

! ensureBlock
  ^ [Transcript show: 'ensureBlock value'. #EnsureBlockValue]
! exceptionHandlerBlock
  ^ [Transcript show: 'exceptionHandlerBlock value'. #ExceptionHandlerBlock]

! start
  | res |
  Transcript show: 'start start'.
  res := self mainBlock on: Error do: self exceptionHandlerBlock.
  Transcript show: 'start end'.
  Transcript show: res.
  ^ res
!!

Bexp new start.

"Transcript show: (Playground new holla: 6) value."
99 halt.

